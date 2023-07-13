# Corrida general del workflow
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

# corrida de cada paso del workflow
# primeros pasos, relativamente rapidos
source("~/labo2023r/src/workflow-inicial/EX_611_CA_reparar_dataset.r")
source("~/labo2023r/src/workflow-inicial/EX_621_DR_corregir_drifting.r")
source("~/labo2023r/src/workflow-inicial/EX_631_FE_historia.r")
source("~/labo2023r/src/workflow-inicial/EX_Trainst-08.r")

# ultimos pasos, muy lentos
source("~/labo2023r/src/workflow-inicial/EX_651_HT_lightgbm.r")
source("~/labo2023r/src/workflow-inicial/EX_661_ZZ_final.r")