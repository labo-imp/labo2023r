# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

setwd("~/buckets/b1/") # establezco la carpeta donde voy a trabajar

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/EA4870/", showWarnings = FALSE)
setwd("./exp/EA4870")


# uso esta semilla para los canaritos
set.seed(102191)

# agrego tantos canaritos como variables tiene el dataset
for (i in 1:ncol(dataset)) {
    dataset[, paste0("canarito", i) := runif(nrow(dataset))]
}

dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]

# Dejo crecer el arbol sin ninguna limitacion
# sin limite de altura ( 30 es el maximo que permite rpart )
# sin limite de minsplit ( 2 es el minimo natural )
# sin limite de minbukcet( 1 es el minimo natural )
# los canaritos me protegeran
modelo_original <- rpart(
    formula = "clase_ternaria ~ .",
    data = dtrain,
    model = TRUE,
    xval = 0,
    cp = -1,
    minsplit = 2, # dejo que crezca y corte todo lo que quiera
    minbucket = 1,
    maxdepth = 30
)


# hago el pruning de los canaritos
# haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[
    modelo_original$frame$var %like% "canarito",
    "complexity"
] <- -666

modelo_pruned <- prune(modelo_original, -666)

prediccion <- predict(modelo_pruned, dapply, type = "prob")[, "BAJA+2"]

entrega <- as.data.table(list(
    "numero_de_cliente" = dapply$numero_de_cliente,
    "Predicted" = as.integer(prediccion > 0.025)
))

fwrite(entrega, paste0("stopping_at_canaritos.csv"), sep = ",")

pdf(file = "stopping_at_canaritos.pdf", width = 28, height = 4)
prp(modelo_pruned,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)

dev.off()
