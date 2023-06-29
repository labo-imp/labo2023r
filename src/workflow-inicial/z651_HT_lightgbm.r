# Experimentos Colaborativos Default
# Hyperparameter Tuning  lightgbm

# pensado para datasets con UNDERSAPLING de la clase mayoritaria

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("rlist")
require("yaml")

require("lightgbm")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

#------------------------------------------------------------------------------
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})
#------------------------------------------------------------------------------

# Parametros del script
PARAM <- list()
PARAM$experimento <- "HT6510"

PARAM$exp_input <- "TS6410"

# En caso que se haga cross validation, se usa esta cantidad de folds
PARAM$lgb_crossvalidation_folds <- 5

PARAM$lgb_semilla <- 102191 # cambiar por su propia semilla


# Hiperparametros FIJOS de  lightgbm
PARAM$lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO
  num_iterations = 9999, # un numero muy grande, lo limita early_stopping_rounds

  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0

  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

  extra_trees = TRUE, # Magic Sauce

  seed = PARAM$lgb_semilla
)


# Aqui se cargan los hiperparametros que se optimizan
#  en la Bayesian Optimization
PARAM$bo_lgb <- makeParamSet(
  makeNumericParam("learning_rate", lower = 0.02, upper = 0.3),
  makeNumericParam("feature_fraction", lower = 0.01, upper = 1.0),
  makeIntegerParam("num_leaves", lower = 8L, upper = 1024L),
  makeIntegerParam("min_data_in_leaf", lower = 100L, upper = 50000L)
)


# si usted es ambicioso, y tiene paciencia, podria subir este valor a 100
PARAM$bo_iteraciones <- 50 # iteraciones de la Optimizacion Bayesiana

PARAM$home <- "~/buckets/b1/"

# FIN Parametros del script

OUTPUT <- list()


#------------------------------------------------------------------------------

GrabarOutput <- function() {
  write_yaml(OUTPUT, file = "output.yml") # grabo OUTPUT
}
#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

exp_log <- function(
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
GLOBAL_arbol <- 0L
GLOBAL_gan_max <- -Inf
vcant_optima <- c()

fganancia_lgbm_meseta <- function(probs, datos) {
  vlabels <- get_field(datos, "label")
  vpesos <- get_field(datos, "weight")


  GLOBAL_arbol <<- GLOBAL_arbol + 1
  tbl <- as.data.table(list(
    "prob" = probs,
    "gan" = ifelse(vlabels == 1 & vpesos > 1, 117000, -3000)
  ))

  setorder(tbl, -prob)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]

  tbl[, gan_suavizada :=
    frollmean(
      x = gan_acum, n = 2001, align = "center",
      na.rm = TRUE, hasNA = TRUE
    )]

  gan <- tbl[, max(gan_suavizada, na.rm = TRUE)]


  pos <- which.max(tbl[, gan_suavizada])
  vcant_optima <<- c(vcant_optima, pos)

  if (GLOBAL_arbol %% 10 == 0) {
    if (gan > GLOBAL_gan_max) GLOBAL_gan_max <<- gan

    cat("\r")
    cat(
      "Validate ", GLOBAL_iteracion, " ", " ",
      GLOBAL_arbol, "  ", gan, "   ", GLOBAL_gan_max, "   "
    )
  }


  return(list(
    "name" = "ganancia",
    "value" = gan,
    "higher_better" = TRUE
  ))
}
#------------------------------------------------------------------------------

EstimarGanancia_lightgbm <- function(x) {
  gc()
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1L
  OUTPUT$BO$iteracion_actual <<- GLOBAL_iteracion
  GrabarOutput()

  # hago la union de los parametros basicos y los moviles que vienen en x
  param_completo <- c(PARAM$lgb_basicos, x)

  param_completo$early_stopping_rounds <-
    as.integer(400 + 4 / param_completo$learning_rate)

  GLOBAL_arbol <<- 0L
  GLOBAL_gan_max <<- -Inf
  vcant_optima <<- c()
  set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  modelo_train <- lgb.train(
    data = dtrain,
    valids = list(valid = dvalidate),
    eval = fganancia_lgbm_meseta,
    param = param_completo,
    verbose = -100
  )

  cat("\n")

  cant_corte <- vcant_optima[modelo_train$best_iter]

  # aplico el modelo a testing y calculo la ganancia
  prediccion <- predict(
    modelo_train,
    data.matrix(dataset_test[, campos_buenos, with = FALSE])
  )

  tbl <- copy(dataset_test[, list("gan" = ifelse(clase_ternaria == "BAJA+2", 117000, -3000))])

  tbl[, prob := prediccion]
  setorder(tbl, -prob)
  tbl[, gan_acum := cumsum(gan)]
  tbl[, gan_suavizada := frollmean(
    x = gan_acum, n = 2001,
    align = "center", na.rm = TRUE, hasNA = TRUE
  )]


  ganancia_test <- tbl[, max(gan_suavizada, na.rm = TRUE)]

  cantidad_test_normalizada <- which.max(tbl[, gan_suavizada])

  rm(tbl)
  gc()

  ganancia_test_normalizada <- ganancia_test


  # voy grabando las mejores column importance
  if (ganancia_test_normalizada > GLOBAL_ganancia) {
    GLOBAL_ganancia <<- ganancia_test_normalizada
    tb_importancia <- as.data.table(lgb.importance(modelo_train))

    fwrite(tb_importancia,
      file = paste0("impo_", sprintf("%03d", GLOBAL_iteracion), ".txt"),
      sep = "\t"
    )

    rm(tb_importancia)
    OUTPUT$BO$mejor$iteracion <<- GLOBAL_iteracion
    OUTPUT$BO$mejor$ganancia <<- GLOBAL_ganancia
    OUTPUT$BO$mejor$arboles <<- modelo_train$best_iter
    GrabarOutput()
  }


  # logueo final
  ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
  xx <- c(ds, copy(param_completo))

  xx$early_stopping_rounds <- NULL
  xx$num_iterations <- modelo_train$best_iter
  xx$estimulos <- cantidad_test_normalizada
  xx$ganancia <- ganancia_test_normalizada
  xx$iteracion_bayesiana <- GLOBAL_iteracion

  exp_log(xx, arch = "BO_log.txt")

  set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------
# esta es la funcion mas mistica de toda la asignatura
# sera explicada en  Laboratorio de Implementacion III

vcant_optima <- c()

fganancia_lgbm_mesetaCV <- function(probs, datos) {
  vlabels <- get_field(datos, "label")
  vpesos <- get_field(datos, "weight")

  GLOBAL_arbol <<- GLOBAL_arbol + 1L

  tbl <- as.data.table(list(
    "prob" = probs,
    "gan" = ifelse(vlabels == 1 & vpesos > 1,
      117000,
      -3000
    )
  ))

  setorder(tbl, -prob)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]
  tbl[, gan_suavizada := frollmean(
    x = gan_acum, n = 501,
    align = "center", na.rm = TRUE, hasNA = TRUE
  )]

  gan <- tbl[, max(gan_suavizada, na.rm = TRUE)]

  pos <- which.max(tbl[, gan_suavizada])

  vcant_optima <<- c(vcant_optima, pos)

  if (GLOBAL_arbol %% (10 * PARAM$lgb_crossvalidation_folds) == 0) {
    if (gan > GLOBAL_gan_max) GLOBAL_gan_max <<- gan

    cat("\r")
    cat(
      "Cross Validate ", GLOBAL_iteracion, " ", " ",
      as.integer(GLOBAL_arbol / PARAM$lgb_crossvalidation_folds), "  ",
      gan * PARAM$lgb_crossvalidation_folds, "   ",
      GLOBAL_gan_max * PARAM$lgb_crossvalidation_folds, "   "
    )
  }

  return(list(
    "name" = "ganancia",
    "value" = gan,
    "higher_better" = TRUE
  ))
}
#------------------------------------------------------------------------------

EstimarGanancia_lightgbmCV <- function(x) {
  gc()
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1L
  OUTPUT$BO$iteracion_actual <<- GLOBAL_iteracion
  GrabarOutput()

  param_completo <- c(PARAM$lgb_basicos, x)

  param_completo$early_stopping_rounds <-
    as.integer(400 + 4 / param_completo$learning_rate)

  vcant_optima <<- c()
  GLOBAL_arbol <<- 0L
  GLOBAL_gan_max <<- -Inf
  set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  modelocv <- lgb.cv(
    data = dtrain,
    eval = fganancia_lgbm_mesetaCV,
    param = param_completo,
    stratified = TRUE, # sobre el cross validation
    nfold = PARAM$lgb_crossvalidation_folds,
    verbose = -100
  )

  cat("\n")

  desde <- (modelocv$best_iter - 1) * PARAM$lgb_crossvalidation_folds + 1
  hasta <- desde + PARAM$lgb_crossvalidation_folds - 1

  cant_corte <- as.integer(mean(vcant_optima[desde:hasta]) *
    PARAM$lgb_crossvalidation_folds)

  ganancia <- unlist(modelocv$record_evals$valid$ganancia$eval)[modelocv$best_iter]
  ganancia_normalizada <- ganancia * PARAM$lgb_crossvalidation_folds


  if (ktest == TRUE) {
    # debo recrear el modelo
    param_completo$early_stopping_rounds <- NULL
    param_completo$num_iterations <- modelocv$best_iter

    modelo <- lgb.train(
      data = dtrain,
      param = param_completo,
      verbose = -100
    )

    # aplico el modelo a testing y calculo la ganancia
    prediccion <- predict(
      modelo,
      data.matrix(dataset_test[, campos_buenos, with = FALSE])
    )

    tbl <- copy(dataset_test[
      ,
      list("gan" = ifelse(clase_ternaria == "BAJA+2", 117000, -3000))
    ])

    tbl[, prob := prediccion]
    setorder(tbl, -prob)

    tbl[, gan_acum := cumsum(gan)]
    tbl[, gan_suavizada := frollmean(
      x = gan_acum, n = 2001,
      align = "center", na.rm = TRUE, hasNA = TRUE
    )]


    # Dato que hay testing, estos valores son ahora los oficiales
    ganancia_normalizada <- tbl[, max(gan_suavizada, na.rm = TRUE)]
    cant_corte <- which.max(tbl[, gan_suavizada])

    rm(tbl)
    gc()
  }



  # voy grabando las mejores column importance
  if (ganancia_normalizada > GLOBAL_ganancia) {
    GLOBAL_ganancia <<- ganancia_normalizada

    param_impo <- copy(param_completo)
    param_impo$early_stopping_rounds <- 0
    param_impo$num_iterations <- modelocv$best_iter

    modelo <- lgb.train(
      data = dtrain,
      param = param_impo,
      verbose = -100
    )

    tb_importancia <- as.data.table(lgb.importance(modelo))

    fwrite(tb_importancia,
      file = paste0("impo_", GLOBAL_iteracion, ".txt"),
      sep = "\t"
    )

    rm(tb_importancia)

    OUTPUT$BO$mejor$iteracion <<- GLOBAL_iteracion
    OUTPUT$BO$mejor$ganancia <<- GLOBAL_ganancia
    OUTPUT$BO$mejor$arboles <<- modelocv$best_iter
    GrabarOutput()
  }


  # logueo final
  ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
  xx <- c(ds, copy(param_completo))

  xx$early_stopping_rounds <- NULL
  xx$num_iterations <- modelocv$best_iter
  xx$estimulos <- cant_corte
  xx$ganancia <- ganancia_normalizada
  xx$iteracion_bayesiana <- GLOBAL_iteracion

  exp_log(xx, arch = "BO_log.txt")
  set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")

  return(ganancia_normalizada)
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
OUTPUT$PARAM <- PARAM
OUTPUT$time$start <- format(Sys.time(), "%Y%m%d %H%M%S")

setwd(PARAM$home)

# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset_training.csv.gz
dataset_input <- paste0("./exp/", PARAM$exp_input, "/dataset_training.csv.gz")
dataset <- fread(dataset_input)

dataset[, azar := NULL]

# Verificaciones
if (!("fold_train" %in% colnames(dataset))) {
  stop("Error, el dataset no tiene el campo fold_train \n")
}

if (!("fold_validate" %in% colnames(dataset))) {
  stop("Error, el dataset no tiene el campo fold_validate \n")
}

if (!("fold_test" %in% colnames(dataset))) {
  stop("Error, el dataset no tiene el campo fold_test  \n")
}

if (dataset[fold_train == 1, .N] == 0) {
  stop("Error, en el dataset no hay registros con fold_train==1 \n")
}

# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

GrabarOutput()
write_yaml(PARAM, file = "parametros.yml") # escribo parametros utilizados

cat(PARAM$exp_input,
  file = "TrainingStrategy.txt",
  append = FALSE
)

# defino la clase binaria clase01
dataset[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]


# los campos que se pueden utilizar para la prediccion
campos_buenos <- setdiff(
  copy(colnames(dataset)),
  c("clase01", "clase_ternaria", "fold_train", "fold_validate", "fold_test")
)

# la particion de train siempre va
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[fold_train == 1, campos_buenos, with = FALSE]),
  label = dataset[fold_train == 1, clase01],
  weight = dataset[
    fold_train == 1,
    ifelse(clase_ternaria == "BAJA+2", 1.0000001,
      ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0)
    )
  ],
  free_raw_data = FALSE
)

OUTPUT$train$ncol <- ncol(dtrain)
OUTPUT$train$nrow <- nrow(dtrain)
OUTPUT$train$periodos <- dataset[fold_train == 1, length(unique(foto_mes))]

kvalidate <- FALSE
ktest <- FALSE
kcrossvalidation <- TRUE

# Si hay que hacer validacion
if (dataset[fold_train == 0 & fold_test == 0 & fold_validate == 1, .N] > 0) {
  kcrossvalidation <- FALSE
  kvalidate <- TRUE
  dvalidate <- lgb.Dataset(
    data = data.matrix(dataset[fold_validate == 1, campos_buenos, with = FALSE]),
    label = dataset[fold_validate == 1, clase01],
    weight = dataset[
      fold_validate == 1,
      ifelse(clase_ternaria == "BAJA+2", 1.0000001,
        ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0)
      )
    ],
    free_raw_data = FALSE
  )

  OUTPUT$validate$ncol <- ncol(dvalidate)
  OUTPUT$validate$nrow <- nrow(dvalidate)

  OUTPUT$validate$periodos <- dataset[
    fold_validate == 1,
    length(unique(foto_mes))
  ]
}


# Si hay que hacer testing
if (dataset[fold_train == 0 & fold_validate == 0 & fold_test == 1, .N] > 0) {
  ktest <- TRUE
  campos_buenos_test <- setdiff(
    copy(colnames(dataset)),
    c("fold_train", "fold_validate", "fold_test")
  )

  dataset_test <- dataset[fold_test == 1, campos_buenos_test, with = FALSE]

  OUTPUT$test$ncol <- ncol(dataset_test)
  OUTPUT$test$nrow <- nrow(dataset_test)
  OUTPUT$test$periodos <- dataset_test[, length(unique(foto_mes))]
}



rm(dataset)
gc()


# si ya existe el archivo log, traigo hasta donde procese
if (file.exists("BO_log.txt")) {
  tabla_log <- fread("BO_log.txt")
  GLOBAL_iteracion <- nrow(tabla_log)
  GLOBAL_ganancia <- tabla_log[, max(ganancia)]
  rm(tabla_log)
} else {
  GLOBAL_iteracion <- 0L
  GLOBAL_ganancia <- -Inf
}


# Aqui comienza la configuracion de mlrMBO


OUTPUT$crossvalidation <- kcrossvalidation
GrabarOutput()

# deobo hacer cross validation o  Train/Validate/Test
if (kcrossvalidation) {
  funcion_optimizar <- EstimarGanancia_lightgbmCV
} else {
  funcion_optimizar <- EstimarGanancia_lightgbm
}


configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar, # la funcion que voy a maximizar
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = TRUE,
  par.set = PARAM$bo_lgb, # definido al comienzo del programa
  has.simple.signature = FALSE # paso los parametros en una lista
)

# archivo donde se graba y cada cuantos segundos
ctrl <- makeMBOControl(
  save.on.disk.at.time = 600,
  save.file.path = "bayesiana.RDATA"
)

ctrl <- setMBOControlTermination(ctrl,
  iters = PARAM$bo_iteraciones
) # cantidad de iteraciones

ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

# establezco la funcion que busca el maximo
surr.km <- makeLearner("regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)

surr.km <- makeLearner("regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  optim.method = "BFGS",
  nugget.estim = TRUE,
  jitter = TRUE,
  control = list(trace = TRUE)
)


# Aqui inicio la optimizacion bayesiana
set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
if (!file.exists("bayesiana.RDATA")) {
  run <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  # si ya existe el archivo RDATA,
  # debo continuar desde el punto hasta donde llegue
  #  usado para cuando se corta la virtual machine
  run <- mboContinue("bayesiana.RDATA") # retomo en caso que ya exista
}

#------------------------------------------------------------------------------
BO_log <- fread("BO_log.txt")
OUTPUT$ganancia_max <- BO_log[, max(ganancia, na.rm = TRUE)]

OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

# dejo la marca final
cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
  file = "zRend.txt",
  append = TRUE
)
