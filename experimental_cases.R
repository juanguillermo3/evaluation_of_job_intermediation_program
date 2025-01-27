#' Title: Worker Participation in Job Intermediation Programs
#' Description: Describes workers' participation in multiple job intermediation programs provided by the agency personnel.

#
# Set Up
# --------

#rm(list=ls())
APP_HOME <- dirname(rstudioapi::getActiveDocumentContext()$path)
DATASETS_HOME <- APP_HOME
setwd(APP_HOME)

#
# Paths
# --------

# Source routes if necessary
# source("routes.R")

#
# app modules
# --------
#

setwd(APP_HOME)
source("dataframe extention.R")

#
# Worker Participation in Job Intermediation Programs
# --------
#

# Load Experimental Cases Function with Renamed Variables
load_experimental_cases <- function(source_file = "experimental cases.xlsx") {
  setwd(APP_HOME)
  readxl::read_excel(source_file) %>%
    dplyr::transmute(
      document_number = doc_num,
      participated_any_activity = activ_cualquiera,
      participated_orientation = activ_orientación,
      participated_skills_training = activ_competencias,
      participated_job_search = activ_búsqueda,
      participated_migrant_support = activ_migrante,
      gender = gender,
      treatment_group = treatment_case,
      prior_activities = previous_activities
    )
}

# View the transformed data
load_experimental_cases() %>% View()

