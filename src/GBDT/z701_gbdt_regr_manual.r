# Gradient Boosting of Decision Trees
# Regresion
# Stumps   , arboles de dos hojas


# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")


PARAM <- list()
PARAM$learning_rate <- 0.3
PARAM$num_iterations <- 5

archivo <- "https://storage.googleapis.com/open-courses/austral2023r-e52a/AustralitosVirtualitos.txt"

#------------------------------------------------------------------------------

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# cargo el dataset
dataset <- fread(archivo)


# las variables independientes del dataset
campos_buenos <- setdiff(colnames(dataset), "Nota")

# el primer error es   ( Nota - promedio )
nota_mean <- dataset[, mean(Nota)]
dataset[, error := Nota - nota_mean]

# almaceno TODOS los splits de todos los arboles
tb_splits <- data.table(
  iter = integer(),
  campo = character(),
  corte = numeric(),
  gain = numeric(),
  mejor = logical()
)

# almaceno el  rmse de los stumps
tb_stumps <- data.table(
  iter = integer(),
  rmse = numeric()
)

tb_stumps <- rbind(
  tb_stumps,
  list(0, dataset[, sqrt(mean(error * error))])
)

# avanzo por las iteraciones
for (iteracion in 1:PARAM$num_iterations) {
  # recorro cada campo
  for (campo in campos_buenos) {
    # me quedo con < campo, error >
    dcolumna <- dataset[
      , list("error" = sum(error)),
      list("valor" = get(campo))
    ]
    # ordeno por los valores
    setorder(dcolumna, valor)

    # calculo los puntos de corte como el punto medio
    dcolumna[, corte := frollmean(valor, 2, align = "left")]

    # calculo el gain
    dcolumna[, gain := cumsum(error)^2]
    pos <- dcolumna[, which.max(abs(gain))]
    reg <- dcolumna[pos]

    # agrego el split a la tabla de splits
    tb_splits <- rbind(
      tb_splits,
      list(
        iteracion,
        campo,
        reg$corte,
        abs(reg$gain),
        FALSE
      )
    )
  }

  # busco el mejor split
  tb_splits[iter == iteracion, mejor := .I == which.max(gain)]
  reg <- tb_splits[iter == iteracion & mejor == TRUE]

  # la hoja contribuye con LR * promedio
  hoja1 <- dataset[get(reg$campo) < reg$corte, mean(error)]
  dataset[get(reg$campo) < reg$corte, delta := -PARAM$learning_rate * hoja1]
  hoja2 <- dataset[is.na(delta), mean(error)]
  dataset[is.na(delta), delta := -PARAM$learning_rate * hoja2]

  # modifico efectivamente el error = Boosting
  dataset[, error := error + delta]
  dataset[, delta := NULL]

  tb_stumps <- rbind(
    tb_stumps,
    list(iteracion, dataset[, sqrt(mean(error * error))])
  )
}


# imprimo
tb_stumps
tb_splits[mejor == TRUE]
