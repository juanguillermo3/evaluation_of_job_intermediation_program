#' Title: Data Sharing and Persistence Utilities
#' Description: Provides tools to facilitate data sharing and persistence between application modules.

library(openxlsx)

#
# Persistence Utilities
# -------

# Function: Share a DataFrame with the Application
# Saves a DataFrame to a specified application route with a backup system.
Dataframe.share_with_app <- function(
    Dataframe,
    label,
    app_route
) {
  file_name <- sprintf("%s.xlsx", label)
  
  # Ensure the application route directory exists
  suppressWarnings(dir.create(app_route))
  
  # Navigate to the application route
  setwd(app_route)
  
  # Backup existing file, if present
  suppressWarnings(file.rename(from = file_name, to = sprintf("%s (backup).xlsx", label)))
  suppressWarnings(file.remove(from = file_name))
  
  # Write the DataFrame to an Excel file
  Dataframe %>%
    openxlsx::write.xlsx(file_name)
  
  # Verify that the file was successfully created
  stopifnot("Error: File not created" = file_name %in% list.files())
}

# Function: Export Statistical Output
# Manages statistical outputs as a module-level singleton and exports to an Excel file.
Dataframe.export_output <- function(
    Dataframe,
    label,
    output_home = getwd(),
    new_output_array = FALSE,
    output_file_name = "output.xlsx"
) {
  # Initialize a new output array if specified
  if (new_output_array) {
    .GlobalEnv[["statistical_output"]] <- list()
  }
  
  # Add the DataFrame to the statistical output list
  .GlobalEnv[["statistical_output"]][[label]] <- Dataframe
  
  # Ensure the output directory exists
  dir.create(output_home)
  setwd(output_home)
  dir.create("output")
  setwd("output")
  
  # Write the statistical output to an Excel file
  openxlsx::write.xlsx(.GlobalEnv[["statistical_output"]], output_file_name)
}

