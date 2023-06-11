# Este script está pensado para correr en Google Cloud
#  8 VCPU  16 GB de memoria RAM

# Aplicacion de los mejores hiperparametros encontrados en una bayesiana
# Utilizando clase_binaria =  [  SI = { "BAJA+1", "BAJA+2"} ,  NO="CONTINUA ]

# cargo las librerias que necesito
require("data.table")
require("rpart")


# para que se detenga ante el primer error
#  y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

PARAM <- list()

PARAM$experimento <- "KA3720"

PARAM$input$dataset <- "./datasets/dataset_pequeno.csv"
# los meses en los que vamos a entrenar
PARAM$input$training <- c(202107)
PARAM$input$future <- c(202109) # meses donde se aplica el modelo

PARAM$rpart$cp <- -0.2618881869
PARAM$rpart$minsplit <- 215.661244913
PARAM$rpart$minbucket <- 9.8589051183
PARAM$rpart$mixdepth <- 8

PARAM$semilla <- 102191

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset
dataset <- fread(PARAM$input$dataset)

# creo la carpeta donde va el experimento
#  HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[
  foto_mes %in% PARAM$input$training,
  clase_binaria := ifelse(clase_ternaria == "CONTINUA", "NO", "SI")
]

dataset[, clase_ternaria := NULL]

# defino donde voy a entrenar
dtrain <- dataset[foto_mes %in% PARAM$input$training]

# defino donde voy a aplicar el modelo
dapply <- dataset[foto_mes %in% PARAM$input$future]


# Entreno el modelo
# obviamente rpart no puede ver  clase_ternaria para predecir  clase_binaria
modelo <- rpart(
  formula = "clase_binaria ~ . ",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  cp = PARAM$rpart$cp,
  minsplit = PARAM$rpart$minsplit,
  minbucket = PARAM$rpart$minbucket,
  maxdepth = PARAM$rpart$mixdepth
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
)

# prediccion es una matriz con DOS columnas, llamadas "NO", "SI"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal <- copy(dapply[, list(numero_de_cliente)])
dfinal[, prob_SI := prediccion[, "SI"]]


# por favor cambiar por una semilla propia
# que sino el Fiscal General va a impugnar la prediccion
set.seed(PARAM$semilla)
dfinal[, azar := runif(nrow(dapply))]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder(dfinal, -prob_SI, azar)


# Aqui viene una parte conceptual fundamental,
#  consecuencia que POS = { BAJA+1, BAJA+2 }
#  ya NO corto por probabilidad, sino que corto cantidad de envios

for (corte in c(8500, 9000, 9500, 10000, 10500, 11000, 11500)) {
  # le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[, Predicted := 0L]
  dfinal[1:corte, Predicted := 1L]


  # solo los campos para Kaggle
  fwrite(dfinal[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento, "_001_", corte, ".csv"),
    sep = ","
  )
}
