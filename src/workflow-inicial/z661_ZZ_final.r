# Experimentos Colaborativos Default

# Workflow  ZZ proceso final con semillas
# Este nuevo script fue desarrolado para ayudar a los alumnos en la realizacion
#  de los experimentos colaborativos
#  y las tareas habituales de comparar el resultado de varias semillas
#  en datos para los que se posee la clase

# SIEMPRE el final train se realiza SIN undersampling

# Acepta un vector de semillas y genera un modelo para cada semilla
# Acepta un vector de modelos_rank; se le puede especificar cuales modelos
#  de la Bayesian Optimzation se quiere
#    modelos_rank es la posición en el ranking, por ganancia descendente
#    ( Atencion,NO es el campo iteracion_bayesiana )
#    por ejemplo, si  PARAM$modelos_rank  <- c( 1 )
# En el caso que el dataset de future posea el campo clase_ternaria con todos los valores,
#  entonces solo genera el mejor modelo
#  se generan los graficos para las semillas en color gris y el promedio en color rojo
# en el caso que se posea clase_completa con sus valores el dataset future,
#  NO se generan los archivos para Kaggle
# en el caso de estar incompleta la clase_ternaria,  se generan los archicos para Kaggle



# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")

require("lightgbm")


# Parametros del script
PARAM <- list()
PARAM$experimento <- "ZZ6610"
PARAM$exp_input <- "HT6510"

# Atencion, que cada modelos se procesa con 5 semillas, ajuste a SUS necesidades
# Que modelos quiero, segun su posicion en el ranking e la Bayesian Optimizacion, ordenado por ganancia descendente
PARAM$modelos_rank <- c(1)

# reemplazar por las propias semillas
PARAM$semillas <- c(102191, 200177, 410551, 552581, 892237)

PARAM$kaggle$envios_desde <- 9500L
PARAM$kaggle$envios_hasta <- 11500L
PARAM$kaggle$envios_salto <- 500L

# para el caso que deba graficar
PARAM$graficar$envios_desde <- 8000L
PARAM$graficar$envios_hasta <- 20000L
PARAM$graficar$ventana_suavizado <- 2001L

PARAM$home <- "~/buckets/b1/"
# FIN Parametros del script

OUTPUT <- list()

#------------------------------------------------------------------------------

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})
#------------------------------------------------------------------------------

GrabarOutput <- function() {
  write_yaml(OUTPUT, file = "output.yml") # grabo OUTPUT
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
OUTPUT$PARAM <- PARAM
OUTPUT$time$start <- format(Sys.time(), "%Y%m%d %H%M%S")

base_dir <- "~/buckets/b1/"

# creo la carpeta donde va el experimento
dir.create(paste0(base_dir, "exp/", PARAM$experimento, "/"),
  showWarnings = FALSE
)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0(base_dir, "exp/", PARAM$experimento, "/"))

GrabarOutput()
write_yaml(PARAM, file = "parametros.yml") # escribo parametros utilizados

# leo la salida de la optimizaciob bayesiana
arch_log <- paste0(base_dir, "exp/", PARAM$exp_input, "/BO_log.txt")
tb_log <- fread(arch_log)
setorder(tb_log, -ganancia)

# leo el nombre del expermento de la Training Strategy
arch_TS <- paste0(base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt")
TS <- readLines(arch_TS, warn = FALSE)

# leo el dataset donde voy a entrenar el modelo final
arch_dataset <- paste0(base_dir, "exp/", TS, "/dataset_train_final.csv.gz")
dataset <- fread(arch_dataset)

# leo el dataset donde voy a aplicar el modelo final
arch_future <- paste0(base_dir, "exp/", TS, "/dataset_future.csv.gz")
dfuture <- fread(arch_future)

# logical que me indica si los datos de future tienen la clase con valores,
# y NO va para Kaggle
future_con_clase <- dfuture[clase_ternaria == "" | is.na(clase_ternaria), .N] == 0

# defino la clase binaria
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+1", "BAJA+2"), 1, 0)]

campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))


# genero un modelo para cada uno de las modelos_qty MEJORES
# iteraciones de la Bayesian Optimization
vganancias_suavizadas <- c()

imodelo <- 0L
for (modelo_rank in PARAM$modelos_rank) {
  imodelo <- imodelo + 1L
  cat("\nmodelo_rank: ", modelo_rank, ", semillas: ")
  OUTPUT$status$modelo_rank <- modelo_rank

  parametros <- as.list(copy(tb_log[modelo_rank]))
  iteracion_bayesiana <- parametros$iteracion_bayesiana


  # creo CADA VEZ el dataset de lightgbm
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[, campos_buenos, with = FALSE]),
    label = dataset[, clase01],
    weight = dataset[, ifelse(clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
    free_raw_data = FALSE
  )

  ganancia <- parametros$ganancia

  # elimino los parametros que no son de lightgbm
  parametros$experimento <- NULL
  parametros$cols <- NULL
  parametros$rows <- NULL
  parametros$fecha <- NULL
  parametros$estimulos <- NULL
  parametros$ganancia <- NULL
  parametros$iteracion_bayesiana <- NULL

  #  parametros$num_iterations  <- 10  # esta linea es solo para pruebas

  if (future_con_clase) {
    tb_ganancias <-
      as.data.table(list("envios" = 1:1:PARAM$graficar$envios_hasta))

    tb_ganancias[, gan_sum := 0.0]
  }

  sem <- 0L

  for (vsemilla in PARAM$semillas)
  {
    sem <- sem + 1L
    cat(sem, " ")
    OUTPUT$status$sem <- sem
    GrabarOutput()

    # Utilizo la semilla definida en este script
    parametros$seed <- vsemilla

    nombre_raiz <- paste0(
      sprintf("%02d", modelo_rank),
      "_",
      sprintf("%03d", iteracion_bayesiana),
      "_s",
      parametros$seed
    )

    arch_modelo <- paste0(
      "modelo_",
      nombre_raiz,
      ".model"
    )

    # genero el modelo entrenando en los datos finales
    set.seed(parametros$seed, kind = "L'Ecuyer-CMRG")
    modelo_final <- lightgbm(
      data = dtrain,
      param = parametros,
      verbose = -100
    )

    # grabo el modelo, achivo .model
    lgb.save(modelo_final,
      file = arch_modelo
    )

    # creo y grabo la importancia de variables
    tb_importancia <- as.data.table(lgb.importance(modelo_final))
    fwrite(tb_importancia,
      file = paste0(
        "impo_",
        nombre_raiz,
        ".txt"
      ),
      sep = "\t"
    )


    # genero la prediccion, Scoring
    prediccion <- predict(
      modelo_final,
      data.matrix(dfuture[, campos_buenos, with = FALSE])
    )

    tb_prediccion <-
      dfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]

    tb_prediccion[, prob := prediccion]


    nom_pred <- paste0(
      "pred_",
      nombre_raiz,
      ".csv"
    )

    fwrite(
      tb_prediccion[, list(numero_de_cliente, foto_mes, prob, clase_ternaria)],
      file = nom_pred,
      sep = "\t"
    )


    # genero los archivos para Kaggle
    cortes <- seq(
      from = PARAM$kaggle$envios_desde,
      to = PARAM$kaggle$envios_hasta,
      by = PARAM$kaggle$envios_salto
    )


    setorder(tb_prediccion, -prob)

    if (!future_con_clase) {
      # genero los archivos para Kaggle
      for (corte in cortes)
      {
        tb_prediccion[, Predicted := 0L]
        tb_prediccion[1:corte, Predicted := 1L]

        nom_submit <- paste0(
          PARAM$experimento,
          "_",
          nombre_raiz,
          "_",
          sprintf("%05d", corte),
          ".csv"
        )

        fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)],
          file = nom_submit,
          sep = ","
        )
      }
    }

    if (future_con_clase) {
      tb_prediccion[, ganancia_acum :=
        cumsum(ifelse(clase_ternaria == "BAJA+2", 117000, -3000))]

      tb_ganancias[, paste0("g", sem) :=
        tb_prediccion[1:PARAM$graficar$envios_hasta, ganancia_acum]]

      tb_ganancias[, gan_sum :=
        gan_sum + tb_prediccion[1:PARAM$graficar$envios_hasta, ganancia_acum]]
    }

    # borro y limpio la memoria para la vuelta siguiente del for
    rm(tb_prediccion)
    rm(tb_importancia)
    rm(modelo_final)
    gc()
  }


  if (future_con_clase) {
    qsemillas <- length(PARAM$semillas)
    tb_ganancias[, gan_sum := gan_sum / qsemillas]

    # calculo la mayor ganancia  SUAVIZADA
    tb_ganancias[, gan_suavizada := frollmean(
      x = gan_sum,
      n = PARAM$graficar$ventana_suavizado,
      align = "center",
      na.rm = TRUE,
      hasNA = TRUE
    )]

    ganancia_suavizada_max <- tb_ganancias[, max(gan_suavizada, na.rm = TRUE)]

    vganancias_suavizadas <- c(vganancias_suavizadas, ganancia_suavizada_max)

    ymax <- max(tb_ganancias, na.rm = TRUE)

    campos_ganancias <- setdiff(colnames(tb_ganancias), "envios")
    ymin <- min(tb_ganancias[envios >= PARAM$graficar$envios_desde, campos_ganancias, with=FALSE ],
      na.rm = TRUE
    )

    arch_grafico <- paste0(
      "modelo_",
      sprintf("%02d", modelo_rank),
      "_",
      sprintf("%03d", iteracion_bayesiana),
      ".pdf"
    )

    pdf(arch_grafico)

    plot(
      x = tb_ganancias[envios >= PARAM$graficar$envios_desde, envios],
      y = tb_ganancias[envios >= PARAM$graficar$envios_desde, g1],
      type = "l",
      col = "gray",
      xlim = c(PARAM$graficar$envios_desde, PARAM$graficar$envios_hasta),
      ylim = c(ymin, ymax),
      main = paste0("Mejor gan prom = ", as.integer(ganancia_suavizada_max)),
      xlab = "Envios",
      ylab = "Ganancia",
      panel.first = grid()
    )

    # las siguientes curvas
    if (qsemillas > 1) {
      for (s in 2:qsemillas)
      {
        lines(
          x = tb_ganancias[envios >= PARAM$graficar$envios_desde, envios],
          y = tb_ganancias[envios >= PARAM$graficar$envios_desde, get(paste0("g", s))],
          col = "gray"
        )
      }
    }

    # finalmente la curva promedio
    lines(
      x = tb_ganancias[envios >= PARAM$graficar$envios_desde, envios],
      y = tb_ganancias[envios >= PARAM$graficar$envios_desde, gan_sum],
      col = "red"
    )

    dev.off()

    # grabo las ganancias, para poderlas comparar con OTROS modelos
    arch_ganancias <- paste0(
      "ganancias_",
      sprintf("%02d", modelo_rank),
      "_",
      sprintf("%03d", iteracion_bayesiana),
      ".txt"
    )

    fwrite(tb_ganancias,
      file = arch_ganancias,
      sep = "\t",
    )

    rm(tb_ganancias)
    gc()
  }

  # impresion ganancias
  rm(dtrain)
  rm(parametros)
  gc()
}

#------------------------------------------------------------------------------
if (future_con_clase) {
  OUTPUT$ganancias_suavizadas <- vganancias_suavizadas
}

OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

# dejo la marca final
cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
  file = "zRend.txt",
  append = TRUE
)
