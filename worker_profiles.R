#'
#' Title: Worker Profiles Module
#' Description: Extracts and processes features from the curricula of workers registered with the job agency.
#'


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
# Profiles
# --------
#

# Worker Profile Features
worker_profile_fields <- c(
  "document_number", 
  "experience", 
  "certification", 
  "foreign_language", 
  "software_skills", 
  "employment_status_at_registration", 
  "digital_skills"
)

# Load Worker Profiles Function with Renamed Variables
load_worker_profiles <- function(source_file = "perfiles laborales.xlsx") {
  readxl::read_excel(source_file) %>%
    dplyr::transmute(
      document_number = num_documento,
      experience = experiencia,
      certification = certificacion,
      foreign_language = idioma_extranjero,
      digital_skills = competencias_Digitales,
      software_skills = software,
      employment_status_at_registration = empleo_al_registro
    ) %>%
    Dataframe.complain_for_vars(worker_profile_fields) %>%
    Dataframe.mandatory_model(worker_profile_fields)
}

# View the transformed data
load_worker_profiles() %>% View()
