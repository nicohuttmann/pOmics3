#' Saves data frames, variables data and observations data as .parquet files 
#' and replaces entries as loading functions understood by get_ functions
#'
#' @param dir folder
#' @param replace should data frames be replaced by loading function 
#' (default = T)
#' @param save should data frames be saved as .parquet file to folder
#' (default = T)
#' @param override.variables Should existing variables data be overwritten? 
#' (default = T)
#' @param override.observations Should existing observations data be overwritten? 
#' (default = T)
#' @param override.data_frames Should existing data frames be overwritten? 
#' (default = F)
#'
#' @return
#' @export
#'
#' 
slim_datasets <- function(dir = "Data/RData/Datasets", 
                          replace = T, 
                          save = T, 
                          override.variables = T, 
                          override.observations = T, 
                          override.data_frames = F) {
  
  # Check folder
  dir.create(dir, showWarnings = F)
  
  ## Save dataset contents in parquet files
  if (save) {
    for (dataset in get_dataset_names()) {
      
      dir.create(file.path(dir, dataset), showWarnings = F)
      
      # Save variables and observations data 
      if (!file.exists(file.path(dir, 
                                 dataset, 
                                 "Variables.parquet")) || 
          override.variables) {
        
        if (!is.function(Datasets[[dataset]][["Variables"]])) {
          
          file.remove(file.path(dir, 
                                dataset, 
                                "Variables.parquet")) %>% 
            suppressWarnings()
          
          arrow::write_parquet(Datasets[[dataset]][["Variables"]], 
                               file.path(dir, 
                                         dataset, 
                                         "Variables.parquet"))
        }
      }
      
      # Observations
      if (!file.exists(file.path(dir, 
                                 dataset, 
                                 "Observations.parquet")) || 
          override.observations) {
        
        if (!is.function(Datasets[[dataset]][["Observations"]])) {
          
          file.remove(file.path(dir, 
                                dataset, 
                                "Observations.parquet")) %>% 
            suppressWarnings()
          
          arrow::write_parquet(Datasets[[dataset]][["Observations"]], 
                               file.path(dir, 
                                         dataset, 
                                         "Observations.parquet"))
        }
      }
      
      # Save data frames
      for (data_frame in get_data_frame_names(dataset = dataset)) {
        
        if (!file.exists(file.path(dir,
                                   dataset,
                                   paste0(data_frame, ".parquet"))) ||
            override.data_frames) {
          
          if (!is.function(Datasets[[dataset]][["Data_frames"]][[data_frame]])) {
            
            file.remove(file.path(dir,
                                  dataset,
                                  paste0(data_frame, ".parquet"))) %>% 
              suppressWarnings()
            
            arrow::write_parquet(
              Datasets[[dataset]][["Data_frames"]][[data_frame]] %>%
                pivot_longer(-1), 
              file.path(dir,
                        dataset,
                        paste0(data_frame, ".parquet")))
          }
        }
      }
    }
  }
  
  
  
  ## Replace data with loading functions 
  if (replace) {
    for (dataset in get_dataset_names()) {
      
      # Variables 
      Datasets[[dataset]][["Variables"]] <<- 
        eval(parse(text = paste0("function() ", 
                                 deparse1(expr(arrow::read_parquet(
                                   file.path(!!dir, 
                                             !!dataset, 
                                             "Variables.parquet")))))), 
             envir = globalenv())
      
      # Observations 
      Datasets[[dataset]][["Observations"]] <<- 
        eval(parse(text = paste0("function() ", 
                                 deparse1(expr(arrow::read_parquet(
                                   file.path(!!dir, 
                                             !!dataset, 
                                             "Observations.parquet")))))), 
             envir = globalenv())
      
      # Data frames 
      for (data_frame in get_data_frame_names(dataset)) {
        
        Datasets[[dataset]][["Data_frames"]][[data_frame]] <<- 
          eval(parse(text = paste0("function() ", 
                                   deparse1(expr(arrow::read_parquet(
                                     file.path(!!dir, 
                                               !!dataset, 
                                               paste0(!!data_frame, ".parquet"))) %>% 
                                       pivot_wider(id_cols = "observations", 
                                                   names_from = "name", 
                                                   values_from = "value"))))), 
               envir = globalenv())
        
      }
    }
  }
}


#' Check if the data frame name exists and if it is already loaded
#'
#' @param which 
#' @param dataset 
#'
#' @return
#' @export
#'
#' @examples
check_data_frame <- function(which, dataset) {
  
  # Checks correct name of dataset
  dataset <- get_dataset(dataset)
  
  # Check if data frame exists
  if (!which %in% get_data_frame_names(dataset = dataset)) {
    
    stop("The data frame name <which> could not be found. Check for spelling ", 
         "or if it has been extracted from the raw data.", call. = FALSE)
  }
  
  # Load data frame if necessary
  if (is.function(Datasets[[dataset]][["Data_frames"]][[which]])) {
    message("Loading data frame from parquet file.")
    Datasets[[dataset]][["Data_frames"]][[which]] <<- 
      Datasets[[dataset]][["Data_frames"]][[which]]()
  }
}


#' Check if the variables has is already loaded
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' 
check_variables <- function(dataset) {
  
  # Checks correct name of dataset
  dataset <- get_dataset(dataset)
  
  # Load variables data if necessary
  if (is.function(Datasets[[dataset]][["Variables"]])) {
    message("Loading variables data from parquet file.")
    Datasets[[dataset]][["Variables"]] <<- 
      Datasets[[dataset]][["Variables"]]()
  }
}


#' Check if the observations has is already loaded
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' 
check_observations <- function(dataset) {
  
  # Checks correct name of dataset
  dataset <- get_dataset(dataset)
  
  # Load observations data if necessary
  if (is.function(Datasets[[dataset]][["Observations"]])) {
    message("Loading observations data from parquet file.")
    Datasets[[dataset]][["Observations"]] <<- 
      Datasets[[dataset]][["Observations"]]()
  }
}


#' Load all data frames into Datasets list
#'
#' @return
#' @export
#'
#' 
thicc_data_frames <- function() {
  for (dataset in get_dataset_names()) {
    for (which in get_data_frame_names()) {
      Datasets[[dataset]][["Data_frames"]][[which]] <<- 
        Datasets[[dataset]][["Data_frames"]][[which]]()
    }
  }
}

#' Load all variables data into Datasets list
#'
#' @return
#' @export
#'
#' 
thicc_variables_data <- function() {
  for (dataset in get_dataset_names()) {
    Datasets[[dataset]][["Variables"]] <<- 
      Datasets[[dataset]][["Variables"]]()
  }
}


#' Load all observaitons data into Datasets list 
#'
#' @return
#' @export
#'
#' 
thicc_observations_data <- function() {
  for (dataset in get_dataset_names()) {
    Datasets[[dataset]][["Observations"]] <<- 
      Datasets[[dataset]][["Observations"]]()
  }
}

#' Load all data frames, variables data, and observaitons data into Datasets 
#' list
#'
#' @return
#' @export
#'
#' 
thicc_datasets <- function() {
  thicc_data_frames()
  thicc_variables_data()
  thicc_observations_data()
}

