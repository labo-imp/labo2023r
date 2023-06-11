# Script para encontrar Visuamente  el data drifting
# focalizado solo en los campos de un buen arbol de deicision


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")

mes1 <- 202108
mes2 <- 202109

#------------------------------------------------------------------------------

graficar_campo <- function(campo) {
  # quito de grafico las colas del 5% de las densidades
  qA <- quantile(dataset[foto_mes == mes1, get(campo)], prob = c(0.05, 0.95), na.rm = TRUE)
  qB <- quantile(dataset[foto_mes == mes2, get(campo)], prob = c(0.05, 0.95), na.rm = TRUE)

  xxmin <- pmin(qA[[1]], qB[[1]])
  xxmax <- pmax(qA[[2]], qB[[2]])

  densidad_A <- density(dataset[foto_mes == mes1, get(campo)],
    kernel = "gaussian", na.rm = TRUE
  )

  densidad_B <- density(dataset[foto_mes == mes2, get(campo)],
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
    legend = c(mes1, mes2),
    col = c("blue", "red"), lty = c(1, 2)
  )
}
#------------------------------------------------------------------------------
# Aqui comienza el programa
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar
dataset <- fread("./datasets/competencia_2023.csv.gz")

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/DR2940/", showWarnings = FALSE)
setwd("./exp/DR2940/")

dataset <- dataset[foto_mes %in% c(mes1, mes2)]


campos_buenos <- setdiff(colnames(dataset), c("numero_de_clientes", "foto_mes", "clase_ternaria"))



pdf(paste0("densidades_", mes1, "_", mes2, ".pdf"))

for (campo in campos_buenos){
  cat(campo, "  ")
  graficar_campo(campo)
}

dev.off()
