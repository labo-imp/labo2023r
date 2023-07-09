# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
# setwd("X:\\gdrive\\austral2023r\\") # Establezco el Working Directory

setwd("C:/SP/Austral/2023/Labo 1/Desktop/Salidas Kaggle/")

# cargo el dataset
dataset <- fread("C:/SP/Austral/2023/Labo 1/datasets/dataset_pequeno.csv")

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

cp0 = -1
minsplit0 = 2222
minbucket0 = 989
maxdepth0 = 6
  
# genero el modelo,  aqui se construye el arbol
modelo <- rpart(
        formula = "clase_ternaria ~ .", # quiero predecir clase_ternaria a partir de el resto de las variables
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = cp0, # esto significa no limitar la complejidad de los splits
        minsplit = minsplit0, # minima cantidad de registros para que se haga el split
        minbucket = minbucket0, # tamaño minimo de una hoja
        maxdepth = maxdepth0
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

archivo <- paste0("cp",gsub("\\.", "dot", gsub("-", "minus", as.character(cp0))),"mb", as.character(minbucket0), "ms",as.character(minsplit0), "mxd", as.character(maxdepth0))

fwrite(dapply[, list(numero_de_cliente, Predicted)], # solo los campos para Kaggle
        file = paste0("./exp/KA2001/", archivo, ".csv"),
        sep = ","
)
