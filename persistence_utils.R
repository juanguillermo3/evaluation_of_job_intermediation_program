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
  suppressWarnings(dir.create(app_route, recursive = TRUE))
  
  # Backup existing file, if present
  backup_file <- file.path(app_route, sprintf("%s (backup).xlsx", label))
  suppressWarnings(file.rename(from = file.path(app_route, file_name), to = backup_file))
  suppressWarnings(file.remove(from = file.path(app_route, file_name)))
  
  # Write the DataFrame to an Excel file within the target directory
  openxlsx::write.xlsx(Dataframe, file.path(app_route, file_name))
  
  # Verify that the file was successfully created
  stopifnot("Error: File not created" = file_name %in% list.files(app_route))
  return(Dataframe)
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
  output_path <- file.path(output_home, "output")
  dir.create(output_path, recursive = TRUE)
  
  # Write the statistical output to an Excel file within the target directory
  openxlsx::write.xlsx(.GlobalEnv[["statistical_output"]], file.path(output_path, output_file_name))
  return(Dataframe)
}


export_ggplot_for_publications <- function(plot, filename, 
                                           width = 8, height = 6, dpi = 900, 
                                           bg = "transparent", dir = APP_HOME) {
  # Ensure the directory exists
  if (!dir.exists(dir)) {
    stop("APP_HOME directory does not exist: ", dir)
  }
  
  # Set working directory
  setwd(dir)
  
  # Save the plot with specified parameters
  ggsave(filename, plot = plot, bg = bg, width = width, height = height, dpi = dpi)
  
  message("Plot successfully saved to ", file.path(dir, filename))
}
