# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

ksemilla_azar <- 102191 # cambiar por la primer semilla

setwd("~/buckets/b1/") # establezco la carpeta donde voy a trabajar

# cargo los datos
dataset <- fread("./datasets/competencia_2023.csv.gz")

# Esta parte se agregar para testear la hipotesis de Rodrigo
# Creo un dataset con TODOS los meses hasta 202107
#  dejo TODOS los BAJA+1 y BAJA+2
#  pero solo un subconjunto de los CONTINUA, asi lo balanceo
set.seed(ksemilla_azar)
dataset[, azar := runif(nrow(dataset))]
dataset <- dataset[foto_mes <= 202107 &
  (clase_ternaria %in% c("BAJA+1", "BAJA+2") | azar < 0.01)]

# elimino el campo azar
dataset[, azar := NULL]

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/EA4850/", showWarnings = FALSE)
setwd("./exp/EA4850")

# uso esta semilla para los canaritos
set.seed(102191)

# agrego 30 variables canarito,
#  random distribucion uniforme en el intervalo [0,1]
for (i in 1:30) dataset[, paste0("canarito", i) := runif(nrow(dataset))]


# Primero  veo como quedan mis arboles
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dataset,
  model = TRUE,
  xval = 0,
  cp = -1.0,
  minsplit = 1365,
  minbucket = 601,
  maxdepth = 10
)


# Grabo el arbol de canaritos
pdf(file = "./arbol_canaritos_Rodrigo.pdf", width = 28, height = 4)
prp(modelo,
  extra = 101, digits = -5,
  branch = 1, type = 4, varlen = 0, faclen = 0
)

dev.off()
