#' Title: Assess Program Balance for Treatment and Control Groups
#' Description: 
#' This function assesses the balance of independent variables across treatment and control groups by performing statistical tests (e.g., T-tests for continuous variables, Proportion tests for binary variables).
#' It calculates the differences between the treatment and control groups, p-values, and adds significance markers (e.g., stars) for each variable. 
#' The output is a data frame with the assessment results, including subsample sizes for both groups.

assess_program_balance <- function(
    dataframe, 
    treatment_var, 
    independent_vars = NULL,
    digits = 4  # Specify rounding precision for numeric results
) {
  # Validate that the treatment variable exists in the dataframe
  if (!treatment_var %in% names(dataframe)) {
    stop(sprintf("The treatment variable '%s' does not exist in the dataframe.", treatment_var))
  }
  
  # If independent_vars is NULL, use all columns except the treatment variable
  if (is.null(independent_vars)) {
    independent_vars <- setdiff(names(dataframe), treatment_var)
  }
  
  # Helper function to apply appropriate statistical tests based on variable type
  apply_statistical_test <- function(variable, treatment) {
    try({
      # Split data by treatment groups
      groups <- split(variable, treatment)
      
      # Proportion test for binary/categorical variables (0/1 or logical)
      if (is.logical(variable) || all(variable %in% c(0, 1))) {
        prop_test <- prop.test(
          x = c(sum(groups[[1]], na.rm = TRUE), sum(groups[[2]], na.rm = TRUE)),
          n = c(length(groups[[1]]), length(groups[[2]]))
        )
        return(data.frame(
          no_treatment = prop_test$estimate[1],
          treatment = prop_test$estimate[2],
          diff = prop_test$estimate[2] - prop_test$estimate[1],
          p_value = prop_test$p.value,
          test = "Proportion Test"
        ))
      } 
      # T-test for numeric variables
      else if (is.numeric(variable)) {
        t_test <- t.test(groups[[1]], groups[[2]])
        return(data.frame(
          no_treatment = t_test$estimate[1],
          treatment = t_test$estimate[2],
          diff = t_test$estimate[2] - t_test$estimate[1],
          p_value = t_test$p.value,
          test = "T-test"
        ))
      } else {
        # For unsupported variable types, return empty dataframe
        return(data.frame())
      }
    }, silent = TRUE)
  }
  
  # Perform balance assessment for each independent variable
  results <- lapply(independent_vars, function(var) {
    test_result <- apply_statistical_test(dataframe[[var]], dataframe[[treatment_var]])
    if (nrow(test_result) > 0) {
      test_result$Variable <- var
    }
    test_result
  }) %>% 
    bind_rows() %>% 
    filter(nrow(.) > 0)  # Remove empty results
  
  # Round numeric columns (mean, difference, p-value) using specified precision
  results <- Dataframe.round_numbers(results, digits)
  
  # Apply star notation for significance based on p-values
  results <- results %>% 
    mutate(
      diff = case_when(
        p_value < 0.01 ~ sprintf("%s***", diff),
        p_value < 0.05 ~ sprintf("%s**", diff),
        p_value < 0.1 ~ sprintf("%s*", diff),
        TRUE ~ sprintf("%s", diff)
      )
    )
  
  # Reorder columns to make "Variable" the first column
  results <- results %>% 
    select(Variable, everything())
  
  # Append subsample sizes for both treatment groups
  subsample_sizes <- table(dataframe[[treatment_var]])
  results <- results %>% 
    bind_rows(
      data.frame(
        Variable = "Subsample Size",
        no_treatment = subsample_sizes[1],
        treatment = subsample_sizes[2],
        diff = NA,
        p_value = NA,
        test = NA
      )
    )
  
  # Remove row names for neatness
  rownames(results) <- NULL
  
  return(results)
}

# Example usage
# assess_balance_for_program_sample(dataframe = my_data, treatment_var = "treatment", independent_vars = c("age", "income", "gender"))


