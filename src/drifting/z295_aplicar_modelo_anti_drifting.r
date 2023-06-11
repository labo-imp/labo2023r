# Genero Arbol sobre dataset  normalizado

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# parmatros experimento
PARAM <- list()
PARAM$experimento <- 2940

# parameetros rpart salen de una Optimizacion Bayesiana
PARAM$rpart_param <- list(
  "cp" = -1,
  "minsplit" = 950,
  "minbucket" = 200,
  "maxdepth" = 8
)


#------------------------------------------------------------------------------

drift_rank_simple <- function(campos_drift) {
  for (campo in campos_drift) {
    cat(campo, " ")
    dataset[, paste0(campo, "_rank") :=
      (frank(get(campo), ties.method = "random") - 1) / (.N - 1), by = foto_mes]

    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------
# El cero se transforma en cero
# los positivos se rankean por su lado
# los negativos se rankean por su lado

drift_rank_cero_fijo <- function(campos_drift) {
  for (campo in campos_drift) {
    cat(campo, " ")
    dataset[get(campo) == 0, paste0(campo, "_rank") := 0]
    dataset[get(campo) > 0, paste0(campo, "_rank") :=
      frank(get(campo), ties.method = "random") / .N, by = foto_mes]

    dataset[get(campo) < 0, paste0(campo, "_rank") :=
      -frank(-get(campo), ties.method = "random") / .N, by = foto_mes]
    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

# la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/KA", PARAM$experimento, "/")
dir.create(paste0("./exp/KA", PARAM$experimento, "/"),
  showWarnings = FALSE
)

setwd(carpeta_experimento)


campos_drift <- setdiff(
  colnames(dataset),
  c("foto_mes", "numero_de_cliente", "clase_ternaria")
)
# drift_rank_cero_fijo( campos_drift )

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# "clase_ternaria ~ ." quiero predecir clase_ternaria a partir
#    del resto de las variables
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  control = PARAM$rpart
)


# grafico el arbol
prp(modelo,
  extra = 101, digits = -5,
  branch = 1, type = 4,
  varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros con probabilidad
# de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
arch_salida <- paste0("K", PARAM$experimento, ".csv")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
  file = arch_salida,
  sep = ","
)
