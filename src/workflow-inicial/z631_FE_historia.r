# Experimentos Colaborativos Default
# Workflow  Feature Engineering historico

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")
require("Rcpp")

require("ranger")
require("randomForest") # solo se usa para imputar nulos

require("lightgbm")


# Parametros del script
PARAM <- list()
PARAM$experimento <- "FE6310"

PARAM$exp_input <- "DR6210"

PARAM$lag1 <- TRUE
PARAM$lag2 <- TRUE
PARAM$lag3 <- FALSE

PARAM$Tendencias1$run <- TRUE
PARAM$Tendencias1$ventana <- 6
PARAM$Tendencias1$tendencia <- TRUE
PARAM$Tendencias1$minimo <- FALSE
PARAM$Tendencias1$maximo <- FALSE
PARAM$Tendencias1$promedio <- FALSE
PARAM$Tendencias1$ratioavg <- FALSE
PARAM$Tendencias1$ratiomax <- FALSE

PARAM$Tendencias2$run <- FALSE
PARAM$Tendencias2$ventana <- 6
PARAM$Tendencias2$tendencia <- TRUE
PARAM$Tendencias2$minimo <- FALSE
PARAM$Tendencias2$maximo <- FALSE
PARAM$Tendencias2$promedio <- FALSE
PARAM$Tendencias2$ratioavg <- FALSE
PARAM$Tendencias2$ratiomax <- FALSE


PARAM$RandomForest$run <- TRUE
PARAM$RandomForest$num.trees <- 20
PARAM$RandomForest$max.depth <- 4
PARAM$RandomForest$min.node.size <- 1000
PARAM$RandomForest$mtry <- 40
PARAM$RandomForest$semilla <- 102191 # cambiar por la propia semilla


# varia de 0.0 a 2.0, si es 0.0 NO se activan
PARAM$CanaritosAsesinos$ratio <- 0.0
# desvios estandar de la media, para el cutoff
PARAM$CanaritosAsesinos$desvios <- 4.0
# cambiar por la propia semilla
PARAM$CanaritosAsesinos$semilla <- 200177

PARAM$home <- "~/buckets/b1/"
# FIN Parametros del script

OUTPUT <- list()

#------------------------------------------------------------------------------

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})
#------------------------------------------------------------------------------

GrabarOutput <- function() {
  write_yaml(OUTPUT, file = "output.yml") # grabo OUTPUT
}
#------------------------------------------------------------------------------
# se calculan para los 6 meses previos el minimo, maximo y
#  tendencia calculada con cuadrados minimos
# la formula de calculo de la tendencia puede verse en
#  https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
# para la maxíma velocidad esta funcion esta escrita en lenguaje C,
# y no en la porqueria de R o Python

cppFunction("NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde )
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 5*n );

  for(int i = 0; i < n; i++)
  {
    //lag
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;


    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) )
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      {
        xsum  += x[h] ;
        ysum  += y[h] ;
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ;
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }

  return  out;
}")

#------------------------------------------------------------------------------
# calcula la tendencia de las variables cols de los ultimos 6 meses
# la tendencia es la pendiente de la recta que ajusta por cuadrados minimos
# La funcionalidad de ratioavg es autoria de  Daiana Sparta,  UAustral  2021

TendenciaYmuchomas <- function(
    dataset, cols, ventana = 6, tendencia = TRUE,
    minimo = TRUE, maximo = TRUE, promedio = TRUE,
    ratioavg = FALSE, ratiomax = FALSE) {
  gc()
  # Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion <- ventana

  last <- nrow(dataset)

  # creo el vector_desde que indica cada ventana
  # de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids <- dataset$numero_de_cliente

  vector_desde <- seq(
    -ventana_regresion + 2,
    nrow(dataset) - ventana_regresion + 1
  )

  vector_desde[1:ventana_regresion] <- 1

  for (i in 2:last) {
    if (vector_ids[i - 1] != vector_ids[i]) {
      vector_desde[i] <- i
    }
  }
  for (i in 2:last) {
    if (vector_desde[i] < vector_desde[i - 1]) {
      vector_desde[i] <- vector_desde[i - 1]
    }
  }

  for (campo in cols) {
    nueva_col <- fhistC(dataset[, get(campo)], vector_desde)

    if (tendencia) {
      dataset[, paste0(campo, "_tend", ventana) :=
        nueva_col[(0 * last + 1):(1 * last)]]
    }

    if (minimo) {
      dataset[, paste0(campo, "_min", ventana) :=
        nueva_col[(1 * last + 1):(2 * last)]]
    }

    if (maximo) {
      dataset[, paste0(campo, "_max", ventana) :=
        nueva_col[(2 * last + 1):(3 * last)]]
    }

    if (promedio) {
      dataset[, paste0(campo, "_avg", ventana) :=
        nueva_col[(3 * last + 1):(4 * last)]]
    }

    if (ratioavg) {
      dataset[, paste0(campo, "_ratioavg", ventana) :=
        get(campo) / nueva_col[(3 * last + 1):(4 * last)]]
    }

    if (ratiomax) {
      dataset[, paste0(campo, "_ratiomax", ventana) :=
        get(campo) / nueva_col[(2 * last + 1):(3 * last)]]
    }
  }
}
#------------------------------------------------------------------------------
# agrega al dataset nuevas variables {0,1}
#  que provienen de las hojas de un Random Forest

AgregaVarRandomForest <- function(
    num.trees, max.depth,
    min.node.size, mtry, semilla) {
  gc()
  dataset[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0, 1)]

  campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria"))

  dataset_rf <- copy(dataset[, campos_buenos, with = FALSE])
  set.seed(semilla, kind = "L'Ecuyer-CMRG")
  azar <- runif(nrow(dataset_rf))

  dataset_rf[, entrenamiento :=
    as.integer(foto_mes >= 202101 & foto_mes <= 202103 &
      (clase01 == 1 | azar < 0.10))]

  # imputo los nulos, ya que ranger no acepta nulos
  # Leo Breiman, ¿por que le temias a los nulos?
  set.seed(semilla, kind = "L'Ecuyer-CMRG")
  dataset_rf <- na.roughfix(dataset_rf)

  campos_buenos <- setdiff(
    colnames(dataset_rf),
    c("clase_ternaria", "entrenamiento")
  )

  set.seed(semilla, kind = "L'Ecuyer-CMRG")
  modelo <- ranger(
    formula = "clase01 ~ .",
    data = dataset_rf[entrenamiento == 1L, campos_buenos, with = FALSE],
    classification = TRUE,
    probability = FALSE,
    num.trees = num.trees,
    max.depth = max.depth,
    min.node.size = min.node.size,
    mtry = mtry,
    seed = semilla,
    num.threads = 1
  )

  rfhojas <- predict(
    object = modelo,
    data = dataset_rf[, campos_buenos, with = FALSE],
    predict.all = TRUE, # entrega la prediccion de cada arbol
    type = "terminalNodes" # entrega el numero de NODO el arbol
  )

  for (arbol in 1:num.trees) {
    hojas_arbol <- unique(rfhojas$predictions[, arbol])

    for (pos in 1:length(hojas_arbol)) {
      # el numero de nodo de la hoja, estan salteados
      nodo_id <- hojas_arbol[pos]
      dataset[, paste0(
        "rf_", sprintf("%03d", arbol),
        "_", sprintf("%03d", nodo_id)
      ) := 0L]

      dataset[
        which(rfhojas$predictions[, arbol] == nodo_id, ),
        paste0(
          "rf_", sprintf("%03d", arbol),
          "_", sprintf("%03d", nodo_id)
        ) := 1L
      ]
    }
  }

  rm(dataset_rf)
  dataset[, clase01 := NULL]

  gc()
}
#------------------------------------------------------------------------------
VPOS_CORTE <- c()

fganancia_lgbm_meseta <- function(probs, datos) {
  vlabels <- get_field(datos, "label")
  vpesos <- get_field(datos, "weight")

  tbl <- as.data.table(list(
    "prob" = probs,
    "gan" = ifelse(vlabels == 1 & vpesos > 1, 117000, -3000)
  ))

  setorder(tbl, -prob)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]
  setorder(tbl, -gan_acum) # voy por la meseta

  gan <- mean(tbl[1:500, gan_acum]) # meseta de tamaño 500

  pos_meseta <- tbl[1:500, median(posicion)]
  VPOS_CORTE <<- c(VPOS_CORTE, pos_meseta)

  return(list(
    "name" = "ganancia",
    "value" = gan,
    "higher_better" = TRUE
  ))
}
#------------------------------------------------------------------------------
# Elimina del dataset las variables que estan por debajo
#  de la capa geologica de canaritos
# se llama varias veces, luego de agregar muchas variables nuevas,
#  para ir reduciendo la cantidad de variables
# y así hacer lugar a nuevas variables importantes

GVEZ <- 1

CanaritosAsesinos <- function(
    canaritos_ratio = 0.2,
    canaritos_desvios = 3.0, canaritos_semilla = 999983) {
  gc()
  dataset[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0, 1)]

  set.seed(canaritos_semilla, kind = "L'Ecuyer-CMRG")
  for (i in 1:(ncol(dataset) * canaritos_ratio)) {
    dataset[, paste0("canarito", i) := runif(nrow(dataset))]
  }

  campos_buenos <- setdiff(
    colnames(dataset),
    c("clase_ternaria", "clase01", "foto_mes")
  )

  set.seed(canaritos_semilla, kind = "L'Ecuyer-CMRG")
  azar <- runif(nrow(dataset))

  dataset[, entrenamiento :=
    foto_mes >= 202101 & foto_mes <= 202103 & (clase01 == 1 | azar < 0.10)]

  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[entrenamiento == TRUE, campos_buenos, with = FALSE]),
    label = dataset[entrenamiento == TRUE, clase01],
    weight = dataset[
      entrenamiento == TRUE,
      ifelse(clase_ternaria == "BAJA+2", 1.0000001, 1.0)
    ],
    free_raw_data = FALSE
  )

  dvalid <- lgb.Dataset(
    data = data.matrix(dataset[foto_mes == 202105, campos_buenos, with = FALSE]),
    label = dataset[foto_mes == 202105, clase01],
    weight = dataset[
      foto_mes == 202105,
      ifelse(clase_ternaria == "BAJA+2", 1.0000001, 1.0)
    ],
    free_raw_data = FALSE
  )


  param <- list(
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    verbosity = -100,
    seed = canaritos_semilla,
    max_depth = -1, # -1 significa no limitar,  por ahora lo dejo fijo
    min_gain_to_split = 0.0, # por ahora, lo dejo fijo
    lambda_l1 = 0.0, # por ahora, lo dejo fijo
    lambda_l2 = 0.0, # por ahora, lo dejo fijo
    max_bin = 31, # por ahora, lo dejo fijo
    num_iterations = 9999, # un numero grande, lo limita early_stopping_rounds
    force_row_wise = TRUE, # para que los alumnos no se atemoricen con  warning
    learning_rate = 0.065,
    feature_fraction = 1.0, # lo seteo en 1
    min_data_in_leaf = 260,
    num_leaves = 60,
    early_stopping_rounds = 200,
    num_threads = 1
  )

  set.seed(canaritos_semilla, kind = "L'Ecuyer-CMRG")
  modelo <- lgb.train(
    data = dtrain,
    valids = list(valid = dvalid),
    eval = fganancia_lgbm_meseta,
    param = param,
    verbose = -100
  )

  tb_importancia <- lgb.importance(model = modelo)
  tb_importancia[, pos := .I]

  fwrite(tb_importancia,
    file = paste0("impo_", GVEZ, ".txt"),
    sep = "\t"
  )

  GVEZ <<- GVEZ + 1

  umbral <- tb_importancia[
    Feature %like% "canarito",
    median(pos) + canaritos_desvios * sd(pos)
  ] # Atencion corto en la mediana mas desvios!!

  col_utiles <- tb_importancia[
    pos < umbral & !(Feature %like% "canarito"),
    Feature
  ]

  col_utiles <- unique(c(
    col_utiles,
    c("numero_de_cliente", "foto_mes", "clase_ternaria", "mes")
  ))

  col_inutiles <- setdiff(colnames(dataset), col_utiles)

  dataset[, (col_inutiles) := NULL]
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
OUTPUT$PARAM <- PARAM
OUTPUT$time$start <- format(Sys.time(), "%Y%m%d %H%M%S")

setwd(PARAM$home)

# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input <- paste0("./exp/", PARAM$exp_input, "/dataset.csv.gz")

dataset <- fread(dataset_input)

colnames(dataset)[which(!(sapply(dataset, typeof) %in% c("integer", "double")))]


# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

GrabarOutput()
write_yaml(PARAM, file = "parametros.yml") # escribo parametros utilizados

#--------------------------------------
# estas son las columnas a las que se puede agregar
#  lags o media moviles ( todas menos las obvias )
cols_lagueables <- copy(setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
))

# ordeno el dataset por <numero_de_cliente, foto_mes> para poder hacer lags
#  es MUY  importante esta linea
setorder(dataset, numero_de_cliente, foto_mes)


if (PARAM$lag1) {
  # creo los campos lags de orden 1
  OUTPUT$lag1$ncol_antes <- ncol(dataset)
  dataset[, paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"),
    by = numero_de_cliente,
    .SDcols = cols_lagueables
  ]

  # agrego los delta lags de orden 1
  for (vcol in cols_lagueables)
  {
    dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
  }

  OUTPUT$lag1$ncol_despues <- ncol(dataset)
  GrabarOutput()
}


cols_lagueables <- intersect(cols_lagueables, colnames(dataset))
if (PARAM$lag2) {
  # creo los campos lags de orden 2
  OUTPUT$lag2$ncol_antes <- ncol(dataset)
  dataset[, paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"),
    by = numero_de_cliente,
    .SDcols = cols_lagueables
  ]

  # agrego los delta lags de orden 2
  for (vcol in cols_lagueables)
  {
    dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
  }

  OUTPUT$lag2$ncol_despues <- ncol(dataset)
  GrabarOutput()
}


cols_lagueables <- intersect(cols_lagueables, colnames(dataset))
if (PARAM$lag3) {
  # creo los campos lags de orden 3
  OUTPUT$lag3$ncol_antes <- ncol(dataset)
  dataset[, paste0(cols_lagueables, "_lag3") := shift(.SD, 3, NA, "lag"),
    by = numero_de_cliente,
    .SDcols = cols_lagueables
  ]

  # agrego los delta lags de orden 3
  for (vcol in cols_lagueables)
  {
    dataset[, paste0(vcol, "_delta3") := get(vcol) - get(paste0(vcol, "_lag3"))]
  }

  OUTPUT$lag3$ncol_despues <- ncol(dataset)
  GrabarOutput()
}


#--------------------------------------
# agrego las tendencias

# ordeno el dataset por <numero_de_cliente, foto_mes> para poder hacer lags
#  es MUY  importante esta linea
setorder(dataset, numero_de_cliente, foto_mes)

cols_lagueables <- intersect(cols_lagueables, colnames(dataset))
if (PARAM$Tendencias1$run) {
  OUTPUT$TendenciasYmuchomas1$ncol_antes <- ncol(dataset)
  TendenciaYmuchomas(dataset,
    cols = cols_lagueables,
    ventana = PARAM$Tendencias1$ventana, # 6 meses de historia
    tendencia = PARAM$Tendencias1$tendencia,
    minimo = PARAM$Tendencias1$minimo,
    maximo = PARAM$Tendencias1$maximo,
    promedio = PARAM$Tendencias1$promedio,
    ratioavg = PARAM$Tendencias1$ratioavg,
    ratiomax = PARAM$Tendencias1$ratiomax
  )

  OUTPUT$TendenciasYmuchomas1$ncol_despues <- ncol(dataset)
  GrabarOutput()
}


cols_lagueables <- intersect(cols_lagueables, colnames(dataset))
if (PARAM$Tendencias2$run) {
  OUTPUT$TendenciasYmuchomas2$ncol_antes <- ncol(dataset)
  TendenciaYmuchomas(dataset,
    cols = cols_lagueables,
    ventana = PARAM$Tendencias2$ventana, # 6 meses de historia
    tendencia = PARAM$Tendencias2$tendencia,
    minimo = PARAM$Tendencias2$minimo,
    maximo = PARAM$Tendencias2$maximo,
    promedio = PARAM$Tendencias2$promedio,
    ratioavg = PARAM$Tendencias2$ratioavg,
    ratiomax = PARAM$Tendencias2$ratiomax
  )

  OUTPUT$TendenciasYmuchomas2$ncol_despues <- ncol(dataset)
  GrabarOutput()
}

#------------------------------------------------------------------------------
# Agrego variables a partir de las hojas de un Random Forest

if (PARAM$RandomForest$run) {
  OUTPUT$AgregaVarRandomForest$ncol_antes <- ncol(dataset)
  AgregaVarRandomForest(
    num.trees = PARAM$RandomForest$num.trees,
    max.depth = PARAM$RandomForest$max.depth,
    min.node.size = PARAM$RandomForest$min.node.size,
    mtry = PARAM$RandomForest$mtry,
    semilla = PARAM$RandomForest$semilla
  )

  OUTPUT$AgregaVarRandomForest$ncol_despues <- ncol(dataset)
  GrabarOutput()
  gc()
}


#--------------------------------------------------------------------------
# Elimino las variables que no son tan importantes en el dataset
# with great power comes grest responsability

if (PARAM$CanaritosAsesinos$ratio > 0.0) {
  OUTPUT$CanaritosAsesinos$ncol_antes <- ncol(dataset)
  CanaritosAsesinos(
    canaritos_ratio = PARAM$CanaritosAsesinos$ratio,
    canaritos_desvios = PARAM$CanaritosAsesinos$desvios,
    canaritos_semilla = PARAM$CanaritosAsesinos$semilla
  )

  OUTPUT$CanaritosAsesinos$ncol_despues <- ncol(dataset)
  GrabarOutput()
}

#------------------------------------------------------------------------------
# grabo el dataset
fwrite(dataset,
  "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
  file = "dataset.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
OUTPUT$dataset$ncol <- ncol(dataset)
OUTPUT$dataset$nrow <- nrow(dataset)

OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

# dejo la marca final
cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
  file = "zRend.txt",
  append = TRUE
)
