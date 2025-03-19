#'
#' Title: sample balance in workers sample 
#' description: merges demographic and professional information from workers, along with participation in job programs, to a single sample. It asses balance in program assignation.
#' image_url:assets/sample_balance.png
#'

#
# Set Up
# --------

rm(list=ls())
APP_HOME <- dirname(rstudioapi::getActiveDocumentContext()$path)
DATASETS_HOME <- APP_HOME
CODE_HOME <- APP_HOME
setwd(APP_HOME)

#
# Paths
# --------

setwd(APP_HOME)
source("routes.R")

#
# custom libraries
# --------
#

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
library(scales)
library(tools)


#
# app modules
# --------
#

setwd(CODE_HOME)
source("assess_program_balance.R")

setwd(CODE_HOME)
source("demographics.R")

setwd(CODE_HOME)
source("worker_profiles.R", encoding="UTF-8")

setwd(CODE_HOME)
source("experimental_cases.R", encoding="UTF-8")

setwd(CODE_HOME)
source("persistence_utils.R", encoding="UTF-8")

setwd(CODE_HOME)
source("dataframe extention.R")


# setwd(CODE_HOME)
# source("genderdize records.R")
# 
# setwd(CODE_HOME)
# source("fechas de registro.R", encoding="UTF-8")
# 
# setwd(CODE_HOME)
# source("data quality control.R")
# db.informacion_de_control() %>% View()


#
# modelo demografico
# --------

load_demographic_model() %>%  head()
# doc_num fecha_registro             tipo_doc genero edad                             estudios     laboral_states es_migrante
# 1   53029594     2020-01-11 Cédula de Ciudadanía  mujer   36                     univ o posterior        desempleado           0
# 2   65709066     2020-01-11 Cédula de Ciudadanía  mujer   38                     univ o posterior        desempleado           0
# 3 1070615064     2020-01-11 Cédula de Ciudadanía  mujer   27                     univ o posterior        desempleado           0
# 4 1024519070     2020-01-11 Cédula de Ciudadanía  mujer   30 desconocida, hasta basica secundaria empleo desconocido           0
# 5 1020752679     2020-01-11 Cédula de Ciudadanía hombre   31             this case shouldnt exist           empleado           0
# 6   51999619     2020-01-12 Cédula de Ciudadanía  mujer   51                     univ o posterior        desempleado           0

#
# intermediacion laboral
# --------

load_experimental_cases() %>% head()
# # A tibble: 6 × 9
# document_number participated_any_activity participated_orienta…¹ participated_skills_…² participated_job_sea…³ participated_migrant…⁴ gender
# <chr>                               <dbl>                  <dbl>                  <dbl>                  <dbl>                  <dbl> <chr> 
#   1 1000001452                              0                      0                      0                      0                      0 mujer 
# 2 1000001562                              1                      1                      0                      0                      0 mujer 
# 3 1000001677                              0                      0                      0                      0                      0 hombre
# 4 1000001805                              2                      1                      0                      1                      0 hombre
# 5 1000001936                              0                      0                      0                      0                      0 hombre
# 6 1000002044                              1                      1                      0                      0                      0 hombre


#
# perfiles laborales
# --------


load_worker_profiles() %>% head()
# # A tibble: 6 × 7
# document_number experience certification foreign_language digital_skills software_skills employment_status_at_registration
# <chr>                <dbl>         <dbl>            <dbl>          <dbl>           <dbl>                             <dbl>
#   1 52812846              1888           384                0              1               1                                 0
# 2 1022932630            1156           596                0              0               0                                 0
# 3 53055643              1126            40                0              0               0                                 0
# 4 52290433              3624           184                1              1               0                                 0
# 5 80913029                 0             0                0              0               0                                 0
# 6 1072428004            1611             0                0              0               0                                 0

#
# unbalanced sample (demographics + treatment status)
# --------


#
# (0)
#
demographic_model=load_demographic_model()
experimental_cases=load_experimental_cases()
worker_profiles=load_worker_profiles()
#
# (1)
#
workers_sample=
dplyr::inner_join(
  demographic_model,
  experimental_cases,
  by="document_number"
  ) %>% 
  dplyr::inner_join(
    worker_profiles,
    by="document_number"
    )
  

#
# analytics
# --------
#

#
# (0)
#
assess_program_balance(
  workers_sample %>%
    dplyr::transmute(
      participated_any_activity=participated_any_activity,
      is_female = gender.x == "female",
      has_high_school = education_level %in% c("high school graduate", "technical")
    )
  ,
  treatment_var = "participated_any_activity"
) %>%
  View()
#
# (1)
#
assessment_of_sample_balance=workers_sample %>%
  transmute(
    
    participated_any_activity=participated_any_activity,
    
    is_female = gender.x == "female",
    has_high_school = education_level %in% c("high school graduate", "technical"),
    has_university_or_higher = education_level %in% c("university or higher"),
    age = age,
    age_under_20 = age < 20,
    age_20_to_30 = age >= 20 & age < 30,
    age_over_30 = age >= 30,
    is_migrant = is_migrant == 1,
    has_experience = experience > 0,
    avg_experience = experience,
    avg_certification = certification,
    speaks_foreign_language = foreign_language == 1,
    has_digital_skills = digital_skills == 1
  ) %>%
  assess_program_balance(treatment_var = "participated_any_activity", standardize = TRUE)
#
assessment_of_sample_balance %>% View()
#
# (2)
#

# Define custom tick label formats
format_proportions <- percent_format(accuracy = 0.1)  # Proportions as percentages
format_continuous <- function(x) sprintf("%.2f", x)   # Continuous values rounded to 2 decimals

# Create plot
plot <- assessment_of_sample_balance %>%
  filter(Variable != "Subsample Size") %>%  # Exclude subsample size row
  mutate(midpoint = pmin(no_treatment, treatment) + (treatment - no_treatment) / 2) %>%  # Compute midpoint
  pivot_longer(cols = c(no_treatment, treatment), names_to = "Group", values_to = "Value") %>%
  mutate(
    Group = recode(Group, "no_treatment" = "Control", "treatment" = "Treatment"),
    Variable = gsub("_", " ", Variable),  # Replace underscores with spaces
    Variable = toTitleCase(Variable),  # Capitalize each word
    midpoint  = ifelse(Group == "Treatment", NA, midpoint),  # Assign NA to Treatment group
    num_diff = as.numeric(gsub("[^0-9.-]", "", diff)),  # Extract numeric difference
    sig_label = gsub("[0-9.-]", "", diff)  # Extract significance symbols
  ) %>%
  ggplot(aes(x = Value, y = Variable, color = Group)) +
  geom_point(size = 3, alpha = 0.4) +  # Plot points for treatment/control groups
  geom_line(aes(group = Variable), linetype = "dashed", color = "gray50", alpha = 0.7) + # Dashed lines
  geom_text_repel(aes(x = midpoint, label = diff), size = 2, color = "black") +  # Midpoint-based text labels
  scale_color_manual(values = c("Control" = "blue", "Treatment" = "red")) +  # Custom colors
  facet_wrap(~ test, scales = "free_x") +  # Facet by test type
  labs(
    title = "Assessment of Sample Balance",
    x = "Measured Value",
    y = "Variable",
    color = "Group"
  ) +
  scale_y_discrete(labels = function(x) gsub("_", " ", toTitleCase(x))) +  # Format y-axis labels
  theme_light(base_family = "Arial") +
  theme(
    text = element_text(family = "Helvetica"),  # Consistent, clean font
    plot.title = element_text(size = 14, hjust = 0.5),  # Centered, bold title
    axis.title = element_text(size = 6),  # Bold axis titles
    axis.text = element_text(size = 6),  # Readable axis labels
    legend.position = "bottom",  # Better legend placement
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 8),
    panel.background = element_rect(fill = alpha("white", 0.5))  # Semi-transparent background
  )

# Save the plot with a transparent background
setwd(APP_HOME)
ggsave("sample_balance.png", plot = plot, bg = "transparent", width = 8, height = 6, dpi = 300)



#
# persist
# --------


#
workers_sample %>%
  Dataframe.share_with_app(label="workers_sample",app_route=WORKERS_SAMPLE_HOME)
#
assestment_of_sample_balance %>%
  Dataframe.export_output(label="assestment_of_sample_balance", output_home=WORKERS_SAMPLE_HOME)


