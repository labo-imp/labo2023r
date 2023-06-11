# esqueleto de grid search
# se espera que los alumnos completen lo que falta para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

# ksemillas <- c(102191, 200177, 410551, 552581, 892237) # reemplazar por las propias semillas
ksemillas <- c(999959, 888887, 555557, 333337, 111119) # reemplazar por las propias semillas

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_ternaria", seed = semilla)

  # genero el modelo
  modelo <- rpart("clase_ternaria ~ .", # quiero predecir clase_ternaria a partir del resto
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
      ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia  tantas veces como valores tenga el vector  ksemillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semillas, # paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = 5
  ) # se puede subir a 5 si posee Linux o Mac OS

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
# setwd("X:\\gdrive\\austral2023r\\") # Establezco el Working Directory
setwd("~/buckets/b1") # Establezco el Working Directory original X:\\gdrive\\austral2023r\\
# cargo los datos

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch.txt"

# Escribo los titulos al archivo donde van a quedar los resultados
# atencion que si ya existe el archivo, esta instruccion LO SOBREESCRIBE, y lo que estaba antes se pierde
# la forma que no suceda lo anterior es con append=TRUE
cat(
  file = archivo_salida,
  sep = "",
  "cp_var", "\t",
  "max_depth", "\t",
  "min_split", "\t",
  "min_bucket", "\t",
  "ganancia_promedio", "\n"
)

# itero por los loops anidados para cada hiperparametro

for (vcp_var in c(-1, -0.5, 0.2, 0, 75, 1)) {
  for (vmax_depth in c(5, 7, 13, 17, 19, 23)) {
    for (vmin_split in c(997, 887,569, 457, 251, 131, 53, 13, 7)) {
      for (vmin_bucket in c(577, 337, 233, 173, 47)) {
        # notar como se agrega
        param_basicos <- list(
          "cp" = vcp_var, # complejidad minima
          "minsplit" = vmin_split, # minima cantidad de registros en un nodo para hacer el split
          "minbucket" = vmin_bucket, # minima cantidad de registros en una hoja
          "maxdepth" = vmax_depth
        ) # profundidad máxima del arbol

        # Un solo llamado, con la semilla 17
        ganancia_promedio <- ArbolesMontecarlo(ksemillas, param_basicos)

        # escribo los resultados al archivo de salida
        cat(
          file = archivo_salida,
          append = TRUE,
          sep = "",
          vcp_var, "\t",
          vmax_depth, "\t",
          vmin_split, "\t",
          vmin_bucket, "\t",
          ganancia_promedio, "\n"
        )
      }
    }
  }
}
