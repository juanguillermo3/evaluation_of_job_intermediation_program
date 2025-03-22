#' Title: Hirings
#' Description: Distributes data about hirings per worker.

#
# Set Up
# --------

#rm(list = ls())
APP_HOME <- dirname(rstudioapi::getActiveDocumentContext()$path)
DATASETS_HOME <- APP_HOME
setwd(APP_HOME)

#
# App Modules
# --------
#

setwd(APP_HOME)
source("dataframe extention.R")

#
# Hirings Model for Workers
# --------
#

# Define demographic model fields
hirings_model_fields <- c()

#
# routes
# --------
#

CODE_HOME=APP_HOME
setwd(CODE_HOME)
source("routes.R")

#
# colcaciones por oferente
# --------
#
library(dplyr)

load_hirings_model=function(){
  setwd(LABORAL_OUTCOMES_HOME)
  readxl::read_excel("laboral_outcomes.xlsx" ) 
}
#
load_hirings_model() %>% View()