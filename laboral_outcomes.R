#' Title: Labor Market Outcomes
#' Description: Summarizes the employment outcomes of workers assisted by the job agency.  
#'              It computes several individual-level metrics, such as the total number of hires,  
#'              as well as hires in high-income positions and permanent contract roles.  
#'              
#' image_path: hiring_rates_per_gender_and_education_level.png


#
# libraries
# --------
#

library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)

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
setwd(CODE_HOME)
source("genderdize records.R")

#
setwd(CODE_HOME)
source("demographics.R")

#
# custom modules (broad use)
# --------
#

setwd(CODE_HOME)
source("dataframe extention.R")

setwd(CODE_HOME)
source("persistence utils.R")


#
# laboral outcomes
# --------
#

#
# 0. Set-up translation for american audiences
#

#
# (1)
#
contract_type_dict <- c(
  "Aprendizaje" = "Apprenticeship",
  "Obra" = "Project-based",
  "Otro" = "Other",
  "Prest. de Servicios" = "Service Contract",
  "Temporal" = "Temporary",
  "Término Fijo" = "Fixed-term",
  "Término Indefinido" = "Indefinite-term"
)
#
# (2)
#
wage_dict <- c(
  "Menos de 1 SMMLV" = "Less than 1 Minimum Wage",
  "1 SMMLV" = "1 Minimum Wage",
  "1 a 2 SMMLV" = "1 to 2 Minimum Wages",
  "2 a 4 SMMLV" = "2 to 4 Minimum Wages",
  "4 a 6 SMMLV" = "4 to 6 Minimum Wages",
  "6 a 9 SMMLV" = "6 to 9 Minimum Wages",
  "15 a 19 SMMLV" = "15 to 19 Minimum Wages",
  "A convenir" = "To be agreed"
)


#
# 1. Load/model the dataset
#

#
# (0) 
#
.handle_special_date_format <- function(
    columna_fechas,
    month_codes = list(
      "(?i)enero" = "01", "(?i)febrero" = "02", "(?i)marzo" = "03",
      "(?i)abril" = "04", "(?i)mayo" = "05", "(?i)junio" = "06",
      "(?i)julio" = "07", "(?i)agosto" = "08", "(?i)septiembre" = "09",
      "(?i)octubre" = "10", "(?i)noviembre" = "11", "(?i)diciembre" = "12"
    ),
    sep = " "
) {
  new_vec <- columna_fechas
  
  # Replace month names with numerical values
  for (rm in names(month_codes)) {
    new_vec <- stringr::str_replace_all(new_vec, rm, month_codes[[rm]])
  }
  
  # Remove non-numeric characters except spaces
  new_vec <- stringr::str_replace_all(new_vec, "[^0-9 ]", "")
  
  # Normalize spaces and trim
  new_vec <- stringr::str_squish(new_vec)
  new_vec <- stringr::str_replace_all(new_vec, " +", sep)
  
  return(new_vec)
}

# Example usage
.handle_special_date_format(
  columna_fechas = c("31 de Octubre de 2020",
                     "7 de Enero de 2021",
                     "12 de Junio de 2021",
                     "17 de Mayo de 2019",
                     "26 de Septiembre de 2017"),
  sep="-"
)
#[1] "31-10-2020" "7-01-2021"  "12-06-2021" "17-05-2019" "26-09-2017"

#
# (1) 
#  
hiring_records <- readxl::read_excel("colocaciones estandarizadas.xlsx") %>% 
  dplyr::transmute(
    document_number = doc_num,
    vacancy_id = proceso,
    wage = salario,
    contract_type = tipo_contrato,
    placement_date = fecha_colocacion
  ) %>% 
  dplyr::mutate(
    placement_date = .handle_special_date_format(placement_date, sep = "-"),
    placement_date = as.Date(placement_date, format = "%d-%m-%Y")
  ) %>% 
  dplyr::mutate(
    # Map translations using plyr::mapvalues
    contract_type = plyr::mapvalues(contract_type, 
                                    from = names(contract_type_dict), 
                                    to = contract_type_dict,
                                    warn_missing = FALSE),
    wage = plyr::mapvalues(wage, 
                           from = names(wage_dict), 
                           to = wage_dict,
                           warn_missing = FALSE)
  ) %>% 
  dplyr::arrange(document_number)
#
hiring_records %>%
  View()


#
# 2. summarize hirings at the level of the individual worker level
#


#
# (0)
#
contract_types=frequency_counts(hiring_records, "contract_type", sort = TRUE, exclude_na = TRUE ) %>% names()
contract_types
# [1] "Fixed-term"       "Project-based"    "Indefinite-term"  "Other"            "Service Contract" "Apprenticeship"  
# [7] "Temporary"
wages=frequency_counts(hiring_records, "wage", sort = TRUE, exclude_na = TRUE ) %>% names() 
wages
# [1] "1 to 2 Minimum Wages"     "1 Minimum Wage"           "2 to 4 Minimum Wages"     "To be agreed"            
# [5] "Less than 1 Minimum Wage" "4 to 6 Minimum Wages"     "6 to 9 Minimum Wages"     "15 to 19 Minimum Wages" 

# #
# # (1.1) naive implementation of the summary (not very efficient)
# #
# Mandatory_vars=c("document_number", "vacancy_id", "wage", "contract_type")
# summary_hirings_per_worker=hiring_records %>% 
# 
#   Dataframe.complain_for_vars(
#     mandatory_vars=Mandatory_vars
#   ) %>%
#   
#   dplyr::mutate(placement_date=as.character(placement_date)) %>%
#   Dataframe.map_NAS(mapped_to = "fake") %>% #View()
#   
#   split(.$document_number)%>%
#   lapply(function(dfxdoc_num){
#     suppressMessages(
#       bind_cols(
#         data.frame(
#           doc_num=dfxdoc_num$document_number[1],
#           num_hirings= sum(dfxdoc_num$vacancy_id!="fake", na.rm=TRUE)
#         ) %>%
#           dplyr::mutate(some_hiring=ifelse(num_hirings>0,1,0)),
#         Dataframe.count_over_factor(dfxdoc_num, counted="contract_type"),
#         Dataframe.count_over_factor(dfxdoc_num, counted="wage")
#       )) 
#   }) %>%
#   bind_rows() 
# #
# summary_hirings_per_worker %>%
#   View()

#
# 3 alternative implementation (more focused on efficiency)
#


#
# (0) filter hiring records to those where an actuall hiring did happen
#
actual_hirings=hiring_records %>% dplyr::filter(!is.na(hiring_records$vacancy_id))
#
# (1) summarize wages in hirings
#
wages_summary=actual_hirings %>% split(.$document_number) %>% #head() %>%
  lapply(function(df){
    list(
      document_number=df$document_number %>% first(),
      frequency_counts(df, "wage")
    ) %>% bind_cols()
    
  }) %>%
  bind_rows() 
#
wages_summary %>%
  View()
#
# (2) summarize contract-types in hirings
#
types_summary=actual_hirings %>% split(.$document_number) %>% #head() %>%
  lapply(function(df){
    list(
      document_number=df$document_number %>% first(),
      frequency_counts(df, "contract_type")
    ) %>% bind_cols()
    
  }) %>%
  bind_rows() 
#
types_summary %>%
  View()
#
# (3) summarize number of hirings per worker
#
overall_summary=actual_hirings %>% split(.$document_number) %>% #head() %>%
  lapply(function(df){
    data.frame(
      document_number=df$document_number %>% first(),
      hiring_sums=nrow(df),
      any_hiring=1
    )
    
  }) %>%
  bind_rows() 
#
overall_summary %>%
  View()
#
# (4) define a workers level dataframe out of the set of workers id´s
#
workers_id=unique(hiring_records$document_number)
summary_per_worker= data.frame(document_number=workers_id)
#
# (5) left join workers data  with wages, types summary on document_number
#

summary_per_worker <- summary_per_worker %>%
  left_join(wages_summary %>% 
              prefix_dataframe_columns("wag_", cols = grep("^doc", names(.), value = TRUE, invert = TRUE) ), 
            by = "document_number") %>%
  left_join(types_summary %>% 
              prefix_dataframe_columns("type_", cols = grep("^doc", names(.), value = TRUE, invert = TRUE) ),  
            by = "document_number") %>%
  left_join(overall_summary, by = "document_number")
# View the combined summary
summary_per_worker %>% View()

#
# (5)
#
summary_per_worker =
  summary_per_worker %>%
  map_NAS()  #View()
table(summary_per_worker$any_hiring)
#
summary_per_worker %>% View()
#
# (6)
#
summary_per_worker %>% names()
summary_per_worker = summary_per_worker %>% curate_names_for_stata()
summary_per_worker %>% names()


#
# 4. discriminating laboral outcomes pre-covid vs post covid
#    (pending of actualizing the code)
#

FECHA_INICIO_COVID
#DATE_COVID_STARTED

#
# 5.
#

#
# (0)
#

#
hirings=summary_per_worker
hirings %>% names()
#
hirings <- hirings %>%
  rowwise_sum(
    sum_columns = c("wag_2_to_4_minimum_wages", 
                    "wag_4_to_6_minimum_wages", 
                    "wag_6_to_9_minimum_wages", 
                    "wag_15_to_19_minimum_wage"), 
    new_col = "high_salary_sum"
  ) %>%
  dplyr::mutate(
    hired = ifelse(hiring_sums > 0, 1, 0),
    high_salary = ifelse(high_salary_sum > 0, 1, 0),
    permanent_contract = ifelse(type_indefinite_term > 0, 1, 0)
  )

#
# (1)
#
setwd(DATASETS_HOME)
demographics=load_demographic_model() 

#
# (2)
#
hiring_rates_per_gender = function(
    hirings = summary_per_worker,
    demographics = load_demographic_model()
){
  # Helper function to format rates: "# (X.XXX%)"
  format_rate <- function(count, total) {
    sprintf("%d (%.3f%%)", count, (count / total) * 100)
  }
  
  # Check required columns for both dataframes
  check_required_columns(hirings, c("document_number", "hired", "high_salary", "permanent_contract"))
  check_required_columns(demographics, c("document_number", "gender"))
  
  hirings %>% 
    dplyr::left_join(demographics, by = "document_number") %>% 
    split(.$gender) %>% 
    lapply(function(gender_df) {
      total <- nrow(gender_df)
      hired_count <- sum(gender_df$hired, na.rm = TRUE)
      high_salary_count <- sum(gender_df$high_salary, na.rm = TRUE)
      permanent_count <- sum(gender_df$permanent_contract, na.rm = TRUE)
      
      data.frame(
        gender = gender_df$gender[[1]],
        sample_size = total,
        placement = format_rate(hired_count, total),
        high_salary = format_rate(high_salary_count, total),
        permanent_contract = format_rate(permanent_count, total)
      )
    }) %>% 
    dplyr::bind_rows()
}
# 
hiring_rates_per_gender(
  hirings,
  demographics 
)

#
# (3)
#

genderized_hiring_rates = function(
    hirings = summary_per_worker,
    demographics = load_demographic_model(),
    group_by = NULL,
    pretty_format = TRUE
){
  # Helper function to format rates: "# (X.XXX%)"
  format_rate <- function(count, total) {
    sprintf("%d (%.3f%%)", count, (count / total) * 100)
  }
  
  # Required columns check
  required_cols <- c("document_number", "gender")
  if (!is.null(group_by)) {
    required_cols <- c(required_cols, group_by)
  }
  check_required_columns(demographics, required_cols)
  check_required_columns(hirings, c("document_number", "hired", "high_salary", "permanent_contract"))
  
  # Join demographics
  joined <- dplyr::left_join(hirings, demographics, by = "document_number")
  
  # Grouping logic
  grouping_vars <- c("gender", group_by) %>% purrr::compact()
  
  result <- joined %>%
    dplyr::group_by(across(all_of(grouping_vars))) %>%
    dplyr::summarize(
      placement_count = sum(hired, na.rm = TRUE),
      high_salary_count = sum(high_salary, na.rm = TRUE),
      permanent_contract_count = sum(permanent_contract, na.rm = TRUE),
      sample_size = n(),
      .groups = "drop"
    )
  
  if (pretty_format) {
    result <- result %>%
      dplyr::mutate(
        placement = format_rate(placement_count, sample_size),
        high_salary = format_rate(high_salary_count, sample_size),
        permanent_contract = format_rate(permanent_contract_count, sample_size)
      ) %>%
      dplyr::select(-c(placement_count, high_salary_count, permanent_contract_count))
  } else {
    result <- result %>%
      dplyr::mutate(
        placement_perc = placement_count / sample_size * 100,
        high_salary_perc = high_salary_count / sample_size * 100,
        permanent_contract_perc = permanent_contract_count / sample_size * 100
      )
  }
  
  return(result)
}
# 
genderized_hiring_rates(
  hirings,
  demographics,
  group_by ="education_level",
  pretty_format = FALSE
)
# 
plot_genderized_hiring_rates <- function(
    hirings = hirings,
    demographics = demographics,
    group_by = NULL
) {
  
  # Call genderized_hiring_rates without the pretty format
  data <- genderized_hiring_rates(
    hirings = hirings,
    demographics = demographics,
    group_by = group_by,
    pretty_format = FALSE
  ) %>%
    na.omit()
  
  # Select necessary columns
  percentage_cols <- c("placement_perc", "high_salary_perc", "permanent_contract_perc")
  selected_data <- data %>%
    dplyr::select(gender, all_of(group_by), dplyr::all_of(percentage_cols)) %>%  
    tidyr::pivot_longer(
      cols = dplyr::all_of(percentage_cols),
      names_to = "outcome",
      values_to = "percentage"
    ) %>%
    mutate(
      gender = str_to_title(gender),
      outcome = str_to_title(str_replace_all(outcome, "_", " "))
    )
  
  # Conditionally apply group_by formatting if it's provided
  if (!is.null(group_by)) {
    selected_data <- selected_data %>%
      mutate(!!group_by := str_wrap(str_to_title(.data[[group_by]]), width = 15))
  }
  
  # Format axis titles
  formatted_x_title <- "Percentage"
  
  # Modify y-axis mapping if group_by is provided
  if (!is.null(group_by)) {
    selected_data <- selected_data %>%
      mutate(!!group_by := str_to_title(str_replace_all(.data[[group_by]], "_", " ")))
  }
  
  formatted_y_title <- ifelse(is.null(group_by), "Gender", str_replace_all(group_by, "_", " ")) %>%
    str_to_title()
  
  # Compute per-facet y-axis range with +5% padding
  facet_y_limits <- selected_data %>%
    group_by(outcome) %>%
    summarise(y_max = max(percentage, na.rm = TRUE) * 1.05)  
  
  # Adjust the title dynamically based on grouping
  plot_title <- "Assessment of Gender-Based Hiring Rates"
  if (!is.null(group_by)) {
    plot_title <- paste0(plot_title, " by ", str_to_title(str_replace_all(group_by, "_", " ")))
  }
  
  # Define base ggplot with consistent styling
  p <- ggplot(selected_data, aes(y = gender, x = percentage, fill = gender)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), alpha = 0.8, width = 0.5, color = "gray50") +  
    geom_text(aes(label = round(percentage, 2)), 
              position = position_dodge(width = 0.3), 
              hjust = -0.2,  
              size = 2.5, 
              color = "gray30",  
              alpha = 0.7) +  
    facet_wrap(~ outcome, scales = "free_x", nrow = 1)  +
    scale_x_continuous(labels = label_number(accuracy = 0.01), expand = expansion(mult = c(0, 0.15))) +
    
    theme_light(base_family = "Arial") +
    theme(
      text = element_text(family = "Helvetica", color = "gray50"),  
      plot.title = element_text(size = 10.5, face = "bold", hjust = 0.5, color = "gray40"),  
      axis.title = element_text(size = 8, color = "gray50"),  
      axis.text = element_text(size = 7, color = "gray50"),  
      legend.position = "bottom",  
      legend.title = element_text(size = 8, color = "gray50"),  
      legend.text = element_text(size = 8, color = "gray50"),
      legend.key.size = unit(0.3, "cm"),
      strip.text = element_text(size = 8, color = "gray40"),  
      panel.background = element_rect(fill = alpha("white", 0.7), color = NA),  
      plot.background = element_rect(fill = alpha("white", 0.7), color = NA)
    )
  
  # Modify y-axis mapping if group_by is provided
  if (!is.null(group_by)) {
    p <- p + aes(y = .data[[group_by]])
  }
  
  p=p + labs(
    title = plot_title,
    x = formatted_x_title,
    y = formatted_y_title,
    fill = "Gender"
    )
  return(p)
}



#
# (1)
#
plot_genderized_hiring_rates(
  hirings,
  demographics
) 
# [1] "gender"     "outcome"    "percentage"
#
# (2)
#
plot_genderized_hiring_rates(
  hirings,
  demographics,
  group_by ="education_level"
) 

# [1] "gender"          "education_level" "outcome"        
# [4] "percentage" 

#
# 5. main
# --------

#
# (0)
#
hirings %>% #View()
  Dataframe.share_with_app(
    "laboral_outcomes",
    DATASETS_HOME
    ) %>%
  View()

#
# (1)
#
hiring_rates_per_gender(
  hirings,
  demographics 
) %>%
Dataframe.export_output(
    "by_gender",
    new_output_array = TRUE,
    output_file_name = "hiring_rates.xlsx"
) %>%
  View()
#
# (2)
#
genderized_hiring_rates(
  hirings,
  demographics,
  group_by ="education_level"
) %>%
  Dataframe.export_output(
    "by_education_level",
    output_file_name = "hiring_rates.xlsx"
  ) %>%
  View()
#
# (3)
#
genderized_hiring_rates(
  hirings, 
  demographics %>% dplyr::mutate(
    age_group = cut(
      age, 
      breaks = seq(15, 75, by = 10),  # 10-year intervals
      right = FALSE,
      labels = paste0(seq(15, 65, by = 10), "-", seq(24, 74, by = 10))  # Adjusted labels
    )), 
  group_by = "age_group"
) %>%
  Dataframe.export_output(
    "age_group",
    output_file_name = "hiring_rates.xlsx"
  ) %>%
  View()

#
# (4) No grouping
#
p <- plot_genderized_hiring_rates(hirings, demographics)
export_ggplot_for_publications(p, "hiring_rates_per_gender.png")
#
# (5) Grouped by education level
#
p <- plot_genderized_hiring_rates(hirings, demographics, group_by = "education_level")
export_ggplot_for_publications(p, "hiring_rates_per_gender_and_education_level.png")
#
# (6) Grouped by age group
#
p <- plot_genderized_hiring_rates(
  hirings, 
  demographics %>% dplyr::mutate(
    age_group = cut(
      age, 
      breaks = seq(15, 75, by = 10),  # 10-year intervals
      right = FALSE,
      labels = paste0(seq(15, 65, by = 10), "-", seq(24, 74, by = 10))  # Adjusted labels
    )
  ), 
  group_by = "age_group"
)
export_ggplot_for_publications(p, "hiring_rates_per_gender_and_age_group.png")


