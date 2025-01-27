#' Title: Demographic Model
#' Description: Provides a demographic model for workers registered with the job agency.

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
# Demographic Model for Workers
# --------
#

# Define demographic model fields
demographic_model_fields <- c(
  "document_number",
  "gender",
  "age",
  "education_level",
  "document_type",
  "employment_status",
  "is_migrant"
)

# Load Demographic Model Function with Renamed Variables
load_demographic_model <- function(source_file = "oferentes_inscritos.xlsx") {
  openxlsx::read.xlsx(source_file) %>%
    dplyr::transmute(
      document_number = documento,
      registration_date = mes_año_registro,
      document_type = tipo_doc,
      gender = ifelse(genero == "F", "female", "male"),
      age = edad,
      education_level = estudios,
      employment_status = laboral_states
    ) %>%
    dplyr::mutate(
      education_level = dplyr::case_when(
        education_level %in% c(
          "educ desconocida",
          "Preescolar",
          "Básica Primaria(1-5)",
          "Básica Secundaria(6-9)"
        ) ~ "unknown or up to secondary",
        education_level %in% c("Media(10-13)") ~ "high school graduate",
        education_level %in% c("Técnica Laboral", "Técnica Profesional", "Tecnológica") ~ "technical",
        education_level %in% c("Universitaria", "Especialización", "Maestría", "Doctorado") ~ "university or higher",
        is.na(education_level) ~ "unknown or up to secondary",
        TRUE ~ "this case shouldn't exist"
      ),
      is_migrant = ifelse(
        document_type %in% c(
          "Cédula de Extranjeria",
          "Documento Nacional de Identificación",
          "Pasaporte",
          "Permiso Especial de Permanencia"
        ), 1, 0
      ),
      employment_status = dplyr::case_when(
        employment_status %in% c("Cesante por Emergencia Sanitaria", "Desempleado") ~ "unemployed",
        employment_status %in% c("Empleado", "Independiente", "Primer Empleo") ~ "employed",
        employment_status %in% c("NO REGISTRA") | is.na(employment_status) ~ "employment unknown",
        TRUE ~ "this case shouldn't exist"
      ),
      registration_date = as.Date(
        stringr::str_extract(registration_date, "[^ ]+"),
        format = "%d/%m/%y"
      )
    ) %>%
    Dataframe.complain_for_vars(demographic_model_fields) %>%
    Dataframe.mandatory_model(demographic_model_fields)
}

# View the transformed data
load_demographic_model() %>%
  View()

