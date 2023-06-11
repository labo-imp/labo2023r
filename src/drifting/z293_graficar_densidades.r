# Script para encontrar Visuamente  el data drifting
# focalizado solo en los campos de un buen arbol de deicision

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")


#------------------------------------------------------------------------------

graficar_campo <- function(campo) {
  # quito de grafico las colas del 5% de las densidades
  qA <- quantile(dataset[foto_mes == 202107, get(campo)],
    prob = c(0.05, 0.95), na.rm = TRUE
  )

  qB <- quantile(dataset[foto_mes == 202109, get(campo)],
    prob = c(0.05, 0.95), na.rm = TRUE
  )

  xxmin <- pmin(qA[[1]], qB[[1]])
  xxmax <- pmax(qA[[2]], qB[[2]])

  densidad_A <- density(dataset[foto_mes == 202107, get(campo)],
    kernel = "gaussian", na.rm = TRUE
  )

  densidad_B <- density(dataset[foto_mes == 202109, get(campo)],
    kernel = "gaussian", na.rm = TRUE
  )

  plot(densidad_A,
    col = "blue",
    xlim = c(xxmin, xxmax),
    ylim = c(0, pmax(max(densidad_A$y), max(densidad_B$y))),
    main = campo
  )

  lines(densidad_B, col = "red", lty = 2)

  legend("topright",
    legend = c("202107", "202109"),
    col = c("blue", "red"), lty = c(1, 2)
  )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar
dataset <- fread("./datasets/dataset_pequeno.csv")

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/DR2930/", showWarnings = FALSE)
setwd("./exp/DR2930/")

dataset <- dataset[foto_mes %in% c(202107, 202109)]

# creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[
  foto_mes == 202107,
  clase_binaria := ifelse(clase_ternaria == "CONTINUA", "NO", "SI")
]

# Entreno el modelo
# utilizo los mejores hiperparametros encontrados
# en una Bayesian Optimizationcon 5-fold Cross Validation
modelo <- rpart(
  formula = "clase_binaria ~ . -clase_ternaria",
  data = dataset[foto_mes == 202107], # los datos donde voy a entrenar
  xval = 0,
  cp = -0.69,
  minsplit = 870,
  minbucket = 9,
  maxdepth = 9
)


campos_modelo <- names(modelo$variable.importance)
campos_buenos <- c(campos_modelo, setdiff(colnames(dataset), campos_modelo))
campos_buenos <- setdiff(
  campos_buenos,
  c("foto_mes", "clase_ternaria", "clase_binaria")
)



pdf("densidades_07_09.pdf")

for (campo in campos_buenos) {
  cat(campo, "  ")
  graficar_campo(campo)
}

dev.off()
