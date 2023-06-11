# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")

setwd("~/buckets/b1/") # establezco la carpeta donde voy a trabajar
# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")


dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/KA4800/", showWarnings = FALSE)
setwd("./exp/KA4800")


dataset[
  foto_mes == 202107,
  clase_binaria := ifelse(clase_ternaria == "BAJA+2", "POS", "NEG")
]

dataset_entrenar <- dataset[foto_mes == 202107]
dataset_aplicar <- dataset[foto_mes == 202109]

# elimino el campo clase ternaria de aplicar
dataset_aplicar[, clase_ternaria := NULL]

for (vmaxdepth in 4:25) {
  # genero el modelo
  modelo <- rpart(
    formula = "clase_binaria ~ . ",
    data = dataset_entrenar[, setdiff(
      colnames(dataset_entrenar),
      c("clase_ternaria")
    ), with = FALSE],
    model = TRUE, # quiero que me devuelva el modelo
    xval = 0,
    cp = 0,
    minsplit = 5,
    maxdepth = vmaxdepth
  )

  # aplico el modelo a los datos en donde entrene
  prediccion_202107 <- predict(
    object = modelo,
    newdata = dataset_entrenar,
    type = "prob"
  )

  ganancia_202107 <- sum((prediccion_202107[, "POS"] > 0.025) *
    ifelse(dataset_entrenar$clase_binaria == "POS", 117000, -3000))

  cat(vmaxdepth, "\t", ganancia_202107, "\n")

  prediccion_202109 <- predict(modelo,
    dataset_aplicar,
    type = "prob"
  )

  prob_pos <- prediccion_202109[, "POS"]
  estimulo <- as.numeric(prob_pos > 0.025)

  entrega <- as.data.table(list(
    "numero_de_cliente" = dataset_aplicar$numero_de_cliente,
    "Predicted" = estimulo
  ))

  # genero el archivo para Kaggle
  fwrite(entrega,
    file = paste0("./altura_", vmaxdepth, ".csv")
  )
}
