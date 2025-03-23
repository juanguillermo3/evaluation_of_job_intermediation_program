#' Title: Merge Workers Dataset with Hiring Rates
#' 
#' Description: This module merges the worker-level dataset (spanning demographics,  
#'              labor background, and program participation status) with the hiring  
#'              rates per worker. The merged dataset is used to provide a basic comparison  
#'              of hiring rates across different engagement levels in the agency program,  
#'              offering a preliminary estimate of the program's effect.  
#'              
#' image_path: assets/naive_*.png  


#
# libraries
# --------
#

library(dplyr)
library(ggplot2)

#
# Set Up
# --------

rm(list=ls())
APP_HOME <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(APP_HOME)

#
# Paths
# --------

setwd(APP_HOME)
source("routes.R")

#
# confg
# --------
#

setwd(CODE_HOME)
source("constants.R")

#
# custom modules (project specific)
# --------
#


#
# custom modules (broad use)
# --------
#

# setwd(CODE_HOME)
# source("dataframe extention.R")

setwd(CODE_HOME)
source("persistence_utils.R")

#
# 1. loading workers datasets and hiting rates
#

#
# (1) workers_sample
#
workers_sample=readxl::read_xlsx(
  file.path(DATASETS_HOME, "workers_sample.xlsx")
) 
workers_sample %>% names()
# [1] "document_number"                    "registration_date"                
# [3] "document_type"                      "gender.x"                         
# [5] "age"                                "education_level"                  
# [7] "employment_status"                  "is_migrant"                       
# [9] "participated_any_activity"          "participated_orientation"         
# [11] "participated_skills_training"      "participated_job_search"          
# [13] "participated_migrant_support"      "gender.y"                         
# [15] "treatment_group"                   "prior_activities"                 
# [17] "experience"                        "certification"                    
# [19] "foreign_language"                  "digital_skills"                   
# [21] "software_skills"                   "employment_status_at_registration"
#
# (2) hiring_records
#
hiring_rates=readxl::read_xlsx(
  file.path(DATASETS_HOME, "laboral_outcomes.xlsx")
) 
hiring_rates %>% names()
# [1] "document_number"           "wag_1_to_2_minimum_wages" 
# [3] "wag_1_minimum_wage"        "wag_less_than_1_minimum_w"
# [5] "wag_4_to_6_minimum_wages"  "wag_to_be_agreed"         
# [7] "wag_2_to_4_minimum_wages"  "wag_6_to_9_minimum_wages" 
# [9] "wag_15_to_19_minimum_wage" "type_fixed_term"          
# [11] "type_project_based"        "type_indefinite_term"     
# [13] "type_other"                "type_service_contract"    
# [15] "type_apprenticeship"       "type_temporary"           
# [17] "hiring_sums"               "any_hiring"               
# [19] "high_salary_sum"           "hired"                    
# [21] "high_salary"               "permanent_contract" 

#
# (3) join approach: left-join from workers sample to hiring rates,
#                    check relative frequency of non-matched cases
#

eval_sample=
  dplyr::left_join(
    workers_sample,
    hiring_rates, 
    by="document_number"
    )
#
prop.table(table(is.na(eval_sample$document_number)))
# FALSE 
# 1

#
# 2. modelling engagement levels, and summarizing hiring rates
#

#
# (0)
#
table(eval_sample$participated_any_activity)
# 0    1    2    3    4    5    6    7    8   12 
# 8307 4570 1105  453  149   69   24    1    1    1 
#
# (1)
#
eval_sample <- eval_sample %>%
  mutate(
    engagement_level = case_when(
      participated_any_activity == 0 ~ "control-group (0)",
      participated_any_activity <= 2 ~ "low-engagement (1-2)",
      participated_any_activity >= 3 ~ "high-engagement (>3)",
      TRUE ~ "quality-check - this record shouldn't exist"
    ),
    engagement_level = factor(
      engagement_level,
      levels = c("control-group (0)", "low-engagement (1-2)", "high-engagement (>3)", "quality-check - this record shouldn't exist"),
      labels = c("Control-Group (0)", "Low-Engagement (1-2)", "High-Engagement (>3)", "quality-check - this record shouldn't exist"),
      ordered = TRUE
    )
  )
#
# (2)
#
rates_by_eng_lev=eval_sample %>%
  split(paste(.$gender.x, .$engagement_level)) %>%
  lapply(function(sub_df) {
    data.frame(
      sex = sub_df$gender.x[1],
      engagement_level = sub_df$engagement_level[1],
      subsample_size = nrow(sub_df),
      total_hirings = sum(sub_df$any_hiring, na.rm = TRUE),
      hiring_rate = round(mean(sub_df$any_hiring, na.rm = TRUE) * 100, 3),
      high_salary_rate = round(mean(sub_df$high_salary, na.rm = TRUE) * 100, 3)
    )
  }) %>%
  bind_rows()
#
# (3) dedicated plot for the naive comparison
#

#
label_positions <- data.frame(
  x = c(1, 2, 3),
  y = rep(7, 3),  # Fixed y position for all labels
  label = c(
    "No treatment", 
    "Selection-Bias dominates", 
    "Program effect dominates")
)
#
naive_comparison_plot=rates_by_eng_lev %>%
  ggplot(aes(x = engagement_level, y = hiring_rate, group = sex, color = sex)) +
  geom_point(aes(size = hiring_rate * 1.2), alpha = 0.5, show.legend = FALSE) +
  geom_line(aes(group = sex), linetype = "dashed") +
  geom_text(aes(label = sprintf("%.3f", hiring_rate)), vjust = -0.8, size = 3) +
  scale_size(range = c(3, 8)) +
  scale_y_continuous(
    limits = c(min(rates_by_eng_lev$hiring_rate) * 0.8, max(rates_by_eng_lev$hiring_rate) * 1.1),
    breaks = seq(floor(min(rates_by_eng_lev$hiring_rate)), ceiling(max(rates_by_eng_lev$hiring_rate)), by = 0.5),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(
    title = "Hiring Rates by Engagement Level",
    x = "Engagement Level",
    y = "Hiring Rate (%)",
    color = "Sex"
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 5, shape = 16, label="")  
  )) +
  theme_light(base_family = "Arial") +
  theme(
    text = element_text(family = "Helvetica", color = "gray50"),
    plot.title = element_text(size = 10.5, face = "bold", hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 8, color = "gray50"),
    axis.text = element_text(size = 7, color = "gray50"),
    legend.title = element_text(size = 8, color = "gray50"),
    legend.text = element_text(size = 8, color = "gray50"),
    legend.key.size = unit(0.3, "cm"),
    strip.text = element_text(size = 8, color = "gray40"),
    panel.background = element_rect(fill = alpha("white", 0.0), color = NA),
    plot.background = element_rect(fill = alpha("white", 0.0), color = NA),
    legend.position = c(0.75, .15),  # Moved legend inside plot (bottom-left)
    legend.justification = c(0.5, 0),  # Align legend bottom-center
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.direction = "horizontal"
  ) +
  geom_rect(aes(
    xmin = 1.5, 
    xmax = 2.5, 
    ymin = -Inf, 
    ymax = Inf
  ),
  fill = "#E0E0E0",
  alpha = 0.1, 
  inherit.aes = FALSE) +
  # Add text labels inside the geom_rect area
  geom_label(data = label_positions, aes(x = x, y = y, label = label),
            fontface = "bold", size = 2, color = "gray30", 
            inherit.aes = FALSE)

#
# 3. main method to share results with the larger application
#

#
# (0)
#
eval_sample %>% 
  Dataframe.share_with_app(
    label="final_evalaution_sample",
    app_route = DATASETS_HOME
  )
#
# (1)
#
rates_by_eng_lev %>%Dataframe.export_output(
  label="dif de medias",
  new_output_array = TRUE,
  output_file_name ="naive_hiring_rates_comparison.xlsx"
  )
#
# (2)
#
export_ggplot_for_publications(naive_comparison_plot, "naive_comparison_plot.png") 







