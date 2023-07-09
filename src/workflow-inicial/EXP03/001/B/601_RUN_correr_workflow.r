# Corrida general del workflow SP  20230706 CORRIDA3

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})


# corrida de cada paso del workflow

# primeros pasos, relativamente rapidos
source("~/labo2023r/src/workflow-inicial/EXP03/001/b/611_CA_reparar_dataset.r")
source("~/labo2023r/src/workflow-inicial/EXP03/001/b/621_DR_corregir_drifting.r")
source("~/labo2023r/src/workflow-inicial/EXP03/001/b/631_FE_historia.r")
source("~/labo2023r/src/workflow-inicial/EXP03/001/b/641_TS_training_strategy.r")

# ultimos pasos, muy lentos
source("~/labo2023r/src/workflow-inicial/EXP03/001/b/651_HT_lightgbm.r")
source("~/labo2023r/src/workflow-inicial/EXP03/001/b/661_ZZ_final.r")


#sp ok 20230706 CORRIDA3