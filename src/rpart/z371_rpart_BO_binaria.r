# Este script esta pensado para corren en Google Cloud
# Una virtual machine con 16GB de RAM es suficiente

# si se lo desea correr en Windows debera
#  * cambiar el setwd()  y las rutas
#  * cuando llame a la funcion mcmapply  poner  mc.cores=1

# Optimizacion Bayesiana de hiperparametros de  rpart
# Hace  1-Repeated  5-Fold Cross Validation


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")

require("rpart")
require("parallel")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")



# parametros experimento
PARAM <- list()
PARAM$experimento <- "HT3710"

PARAM$dataset <- "./datasets/dataset_pequeno.csv"
# los meses en los que entreno, TODOS deben tener la clase completa
PARAM$training_meses <- c(202107)
PARAM$semillas <- c(102191) # Mas de una semilla cambia el  1-Repeated
PARAM$crossvalidation <- 5 # cantidad de folds del cross validation

# cantidad de iteraciones de la optimizacin bayesiana
PARAM$BO$iteraciones <- 150

#------------------------------------------------------------------------------

# la letra L al final de 3L significa ENTERO

hs <- makeParamSet(
  makeNumericParam("cp", lower = -1.0, upper = 0.1),
  makeNumericParam("minsplit", lower = 1, upper = 8000),
  makeNumericParam("minbucket", lower = 1, upper = 4000),
  makeIntegerParam("maxdepth", lower = 3L, upper = 20L),
  forbidden = quote(minbucket > 0.5 * minsplit)
)
# minbucket NO PUEDE ser mayor que la mitad de minsplit


#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(reg, arch = NA, folder = "./work/",
                    ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)

  # Escribo los titulos
  if (!file.exists(archivo)) {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )

    cat(linea, file = archivo)
  }

  # la fecha y hora
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
    gsub(", ", "\t", toString(reg)), "\n"
  )

  # grabo al archivo
  cat(linea, file = archivo, append = TRUE)

  # imprimo por pantalla
  if (verbose) cat(linea)
}
#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset que consiste
#   en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30),
#   agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30
# particionar( data=dataset, division=c(1,1,1,1,1),
#  agrupa=clase_ternaria, seed=semilla)
#  divide el dataset en 5 particiones

particionar <- function(data, division, agrupa = "", campo = "fold",
                        start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(
    function(x, y) {
      rep(y, x)
    },
    division, seq(from = start, length.out = length(division))
  ))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------
# fold_test  tiene el numero de fold que voy a usar para testear,
# entreno en el resto de los folds
# param tiene los hiperparametros del arbol

ArbolSimple <- function(fold_test, data, param) {
  param2 <- param

  # genero el modelo
  # es VITAL que el campo clase_ternaria NO sea visto por el rpart

  # data= data[ fold != fold_test, ]
  #  entreno en todo MENOS el fold_test que uso para testing

  modelo <- rpart("clase_binaria ~ . -clase_ternaria",
    data = data[fold != fold_test, ],
    xval = 0,
    control = param2
  )

  # aplico el modelo a los datos de testing
  # data[ fold==fold_test, ]  aplico el modelo sobre los datos de testing
  prediccion <- predict(modelo,
    data[fold == fold_test, ],
    type = "prob"
  )

  dtest <- copy(data[fold == fold_test, list(clase_ternaria)])
  dtest[, pred := prediccion[, "SI"]]
  ganancia_testing <- dtest[
    pred > 1 / 40,
    sum(ifelse(clase_ternaria == "BAJA+2", 117000, -3000))
  ]

  rm(dtest)

  # esta es la ganancia sobre el fold de testing, NO esta normalizada
  return(ganancia_testing)
}
#------------------------------------------------------------------------------

ArbolesCrossValidation <- function(semilla, data, param, qfolds, pagrupa) {
  divi <- rep(1, qfolds) # generalmente  c(1, 1, 1, 1, 1 )  cinco unos

  # particiono en dataset en folds
  particionar(data, divi, seed = semilla, agrupa = pagrupa)

  ganancias <- mcmapply(ArbolSimple,
    seq(qfolds), # 1 2 3 4 5
    MoreArgs = list(data, param),
    SIMPLIFY = FALSE,
    mc.cores = 5
  )

  data[, fold := NULL]

  # devuelvo la primer ganancia y el promedio
  # promedio las ganancias
  ganancia_promedio <- mean(unlist(ganancias))
  # aqui normalizo la ganancia
  ganancia_promedio_normalizada <- ganancia_promedio * qfolds

  gc()

  return(ganancia_promedio_normalizada)
}
#------------------------------------------------------------------------------
# esta funcion solo puede recibir los parametros que se estan optimizando
# el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia <- function(x) {
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1

  xval_folds <- PARAM$crossvalidation
  vganancias <- mcmapply(ArbolesCrossValidation,
    PARAM$semillas,
    MoreArgs = list(dtrain,
      param = x,
      qfolds = xval_folds,
      pagrupa = "clase_ternaria"
    ),
    SIMPLIFY = FALSE,
    mc.cores = PARAM$crossvalidation
  )

  ganancia_promedio <- mean(unlist(vganancias))

  # logueo
  xx <- x
  xx$xval_folds <- xval_folds
  xx$ganancia <- ganancia_promedio
  xx$iteracion <- GLOBAL_iteracion
  loguear(xx, arch = archivo_log)

  return(xx$ganancia)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

setwd("~/buckets/b1/")

# cargo el dataset, aqui debe poner  SU RUTA
dataset <- fread(PARAM$dataset) # donde entreno

# creo la clase_binaria  SI= {BAJA+1, BAJA+2}  NO={CONTINUA}
dataset[
  foto_mes %in% PARAM$training_meses,
  clase_binaria := ifelse(clase_ternaria == "CONTINUA", "NO", "SI")
]

# defino los datos donde entreno
dtrain <- dataset[foto_mes %in% PARAM$training_meses, ]


# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/", PARAM$experimento, "/")
dir.create(paste0("./exp/", PARAM$experimento, "/"),
  showWarnings = FALSE
)

setwd(carpeta_experimento)
# defino los archivos donde guardo los resultados de la Bayesian Optimization
archivo_log <- paste0(PARAM$experimento, ".txt")
archivo_BO <- paste0(PARAM$experimento, ".RDATA")

# leo si ya existe el log, para retomar en caso que se se corte el programa
GLOBAL_iteracion <- 0

if (file.exists(archivo_log)) {
  tabla_log <- fread(archivo_log)
  GLOBAL_iteracion <- nrow(tabla_log)
}



# Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar <- EstimarGanancia

configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar,
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = TRUE,
  par.set = hs,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl(save.on.disk.at.time = 600, save.file.path = archivo_BO)
ctrl <- setMBOControlTermination(ctrl, iters = PARAM$BO$iteraciones)
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

surr.km <- makeLearner("regr.km",
  predict.type = "se",
  covtype = "matern3_2", control = list(trace = TRUE)
)

# inicio la optimizacion bayesiana
if (!file.exists(archivo_BO)) {
  run <- mbo(
    fun = obj.fun,
    learner = surr.km,
    control = ctrl
  )
} else {
  run <- mboContinue(archivo_BO)
} # retomo en caso que ya exista
