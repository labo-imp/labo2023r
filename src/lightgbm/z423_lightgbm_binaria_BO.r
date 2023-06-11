# Este script esta pensado para correr en Google Cloud
#   8 vCPU
#  16 GB memoria RAM

# se entrena con clase_binaria2  POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm,
# con el metodo TRADICIONAL de los hiperparametros originales de lightgbm
# 5-fold cross validation el cual es muuuy lento
# la probabilidad de corte es un hiperparametro

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")

require("lightgbm")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})



# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()

PARAM$experimento <- "HT4230"

PARAM$input$dataset <- "./datasets/dataset_pequeno.csv"
PARAM$input$training <- c(202107) # los meses en los que vamos a entrenar

# un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$undersampling <- 1.0
PARAM$trainingstrategy$semilla_azar <- 102191 # Aqui poner su  primer  semilla

PARAM$hyperparametertuning$iteraciones <- 100
PARAM$hyperparametertuning$xval_folds <- 5
PARAM$hyperparametertuning$POS_ganancia <- 117000
PARAM$hyperparametertuning$NEG_ganancia <- -3000

# Aqui poner su segunda semilla
PARAM$hyperparametertuning$semilla_azar <- 200177
#------------------------------------------------------------------------------

# Aqui se cargan los bordes de los hiperparametros
hs <- makeParamSet(
  makeNumericParam("learning_rate", lower = 0.01, upper = 0.3),
  makeNumericParam("feature_fraction", lower = 0.2, upper = 1.0),
  makeIntegerParam("min_data_in_leaf", lower = 1L, upper = 8000L),
  makeIntegerParam("num_leaves", lower = 16L, upper = 1024L),
  makeIntegerParam("envios", lower = 5000L, upper = 15000L)
)

#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(
    reg, arch = NA, folder = "./exp/",
    ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)

  if (!file.exists(archivo)) # Escribo los titulos
    {
      linea <- paste0(
        "fecha\t",
        paste(list.names(reg), collapse = "\t"), "\n"
      )

      cat(linea, file = archivo)
    }

  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t", # la fecha y hora
    gsub(", ", "\t", toString(reg)), "\n"
  )

  cat(linea, file = archivo, append = TRUE) # grabo al archivo

  if (verbose) cat(linea) # imprimo por pantalla
}
#------------------------------------------------------------------------------
# esta funcion calcula internamente la ganancia de la prediccion probs
# es llamada por lightgbm luego de construir cada  arbolito

fganancia_logistic_lightgbm <- function(probs, datos) {
  vpesos <- get_field(datos, "weight")

  # vector de ganancias
  vgan <- ifelse(vpesos == 1.0000002, PARAM$hyperparametertuning$POS_ganancia,
    ifelse(vpesos == 1.0000001, PARAM$hyperparametertuning$NEG_ganancia,
      PARAM$hyperparametertuning$NEG_ganancia /
        PARAM$trainingstrategy$undersampling
    )
  )

  tbl <- as.data.table(list("vprobs" = probs, "vgan" = vgan))
  setorder(tbl, -vprobs)
  ganancia <- tbl[1:GLOBAL_envios, sum(vgan)]

  return(list(
    "name" = "ganancia",
    "value" = ganancia,
    "higher_better" = TRUE
  ))
}
#------------------------------------------------------------------------------
# esta funcion solo puede recibir los parametros que se estan optimizando
# el resto de los parametros se pasan como variables globales,
# la semilla del mal ...


EstimarGanancia_lightgbm <- function(x) {
  gc() # libero memoria

  # llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1

  # para usar en fganancia_logistic_lightgbm
  # asigno la variable global
  GLOBAL_envios <<- as.integer(x$envios / PARAM$hyperparametertuning$xval_folds)

  # cantidad de folds para cross validation
  kfolds <- PARAM$hyperparametertuning$xval_folds

  param_basicos <- list(
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    verbosity = -100,
    max_depth = -1, # -1 significa no limitar,  por ahora lo dejo fijo
    min_gain_to_split = 0.0, # por ahora, lo dejo fijo
    lambda_l1 = 0.0, # por ahora, lo dejo fijo
    lambda_l2 = 0.0, # por ahora, lo dejo fijo
    max_bin = 31, # por ahora, lo dejo fijo
    num_iterations = 9999, # valor grande, lo limita early_stopping_rounds
    force_row_wise = TRUE, # para evitar warning
    seed = PARAM$hyperparametertuning$semilla_azar
  )

  # el parametro discolo, que depende de otro
  param_variable <- list(
    early_stopping_rounds =
      as.integer(50 + 5 / x$learning_rate)
  )

  param_completo <- c(param_basicos, param_variable, x)

  set.seed(PARAM$hyperparametertuning$semilla_azar)
  modelocv <- lgb.cv(
    data = dtrain,
    eval = fganancia_logistic_lightgbm,
    stratified = TRUE, # sobre el cross validation
    nfold = kfolds, # folds del cross validation
    param = param_completo,
    verbose = -100
  )

  # obtengo la ganancia
  ganancia <- unlist(modelocv$record_evals$valid$ganancia$eval)[modelocv$best_iter]

  ganancia_normalizada <- ganancia * kfolds # normailizo la ganancia

  # asigno el mejor num_iterations
  param_completo$num_iterations <- modelocv$best_iter
  # elimino de la lista el componente
  param_completo["early_stopping_rounds"] <- NULL

  # Voy registrando la importancia de variables
  if (ganancia_normalizada > GLOBAL_gananciamax) {
    GLOBAL_gananciamax <<- ganancia_normalizada
    modelo <- lgb.train(
      data = dtrain,
      param = param_completo,
      verbose = -100
    )

    tb_importancia <- as.data.table(lgb.importance(modelo))
    archivo_importancia <- paste0("impo_", GLOBAL_iteracion, ".txt")
    fwrite(tb_importancia,
      file = archivo_importancia,
      sep = "\t"
    )
  }


  # el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  # esta es la forma de devolver un parametro extra
  attr(ganancia_normalizada, "extras") <-
    list("num_iterations" = modelocv$best_iter)

  # logueo
  xx <- param_completo
  xx$ganancia <- ganancia_normalizada # le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear(xx, arch = klog)

  return(ganancia_normalizada)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# en estos archivos quedan los resultados
kbayesiana <- paste0(PARAM$experimento, ".RDATA")
klog <- paste0(PARAM$experimento, ".txt")


GLOBAL_iteracion <- 0 # inicializo la variable global
GLOBAL_gananciamax <- -1 # inicializo la variable global

# si ya existe el archivo log, traigo hasta donde llegue
if (file.exists(klog)) {
  tabla_log <- fread(klog)
  GLOBAL_iteracion <- nrow(tabla_log)
  GLOBAL_gananciamax <- tabla_log[, max(ganancia)]
}



# paso la clase a binaria que tome valores {0,1}  enteros
dataset[
  foto_mes %in% PARAM$input$training,
  clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)
]


# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01", "azar", "training")
)

# defino los datos que forma parte del training
# aqui se hace el undersampling de los CONTINUA
set.seed(PARAM$trainingstrategy$semilla_azar)
dataset[, azar := runif(nrow(dataset))]
dataset[, training := 0L]
dataset[
  foto_mes %in% PARAM$input$training &
    (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
  label = dataset[training == 1L, clase01],
  weight = dataset[training == 1L, ifelse(clase_ternaria == "BAJA+2", 1.0000002, ifelse(clase_ternaria == "BAJA+1", 1.0000001, 1.0))],
  free_raw_data = FALSE
)



# Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar <- EstimarGanancia_lightgbm # la funcion que voy a maximizar

configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar, # la funcion que voy a maximizar
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = TRUE,
  par.set = hs, # definido al comienzo del programa
  has.simple.signature = FALSE # paso los parametros en una lista
)

# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
  save.on.disk.at.time = 600, # se graba cada 600 segundos
  save.file.path = kbayesiana
) # se graba cada 600 segundos

# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
  ctrl,
  iters = PARAM$hyperparametertuning$iteraciones
) # cantidad de iteraciones

# defino el método estandar para la creacion de los puntos iniciales,
# los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

# establezco la funcion que busca el maximo
surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)

# inicio la optimizacion bayesiana
if (!file.exists(kbayesiana)) {
  run <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  run <- mboContinue(kbayesiana) # retomo en caso que ya exista
}


cat("\n\nLa optimizacion Bayesiana ha terminado\n")
