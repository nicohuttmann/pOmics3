#' Assemble data frame from dataset 
#'
#' @param which specific name of data type
#' @param variables selected variables
#' @param observations selected observations
#' @param dataset dataset name 
#' @param output.type data type (default = "tibble", "data.frame", "matrix")
#'
#' @return
#' @export
#'
#'
get_data_frame <- function(which,
                           variables,
                           observations,
                           dataset, 
                           output.type = "tibble") {
  
  
  # Default data name
  if (!hasArg(which)) {
    stop("Specify the data frame name with <which>.", call. = FALSE)
  } else if (!is.character(which)) {
    stop("Provide a character string as data frame name with <which>.", 
         call. = FALSE)
  } else if (length(which) != 1) {
    stop("Provide one data frame name with <which>.", call. = FALSE)
  }
  
  
  # Checks correct name of dataset
  dataset <- get_dataset(dataset)
  
  # Assemble variables
  variables <- get_variables(variables = {{variables}},
                             dataset = dataset)
  
  # Assemble observations
  observations <- get_observations(observations = {{observations}},
                                   dataset = dataset)
  
  
  # Check if which exists and load data if not done yet
  check_data_frame(which, dataset)
  
  
  # Grab data
  data <- Datasets[[dataset]][["Data_frames"]][[which]]
  
  
  # Tibble 
  if (grepl(pattern = "tibble", x = output.type)) {
    
    data <- data %>%
      dplyr::filter(observations %in% !!observations) %>%
      dplyr::select(c(observations, dplyr::any_of(variables)))
    
    # Data frame
  } else if (grepl(pattern = "data.frame", x = output.type)) {
    
    data <- data %>%
      dplyr::filter(observations %in% !!observations) %>%
      dplyr::select(c(observations, dplyr::any_of(variables))) %>%
      tibble2data_frame()
    
    # Matrix
  } else if (grepl(pattern = "matrix", x = output.type)) {
    
    data <- data %>%
      dplyr::filter(observations %in% !!observations) %>%
      dplyr::select(c(observations, dplyr::any_of(variables))) %>%
      tibble2matrix()
    
  } else {
    
    stop("Data type <", output.type, 
         "> is not supported. Use <tibble>, <data.frame> ",
         "or <matrix> instead.", 
         call. = FALSE)
    
  }
  
  # Return
  return(data)
  
}


#' Prints or returns data names
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_data_frame_names <- function(dataset) {
  
  # dataset
  dataset <- get_dataset(dataset)
  
  data_frame_names <- names(Datasets[[dataset]][["Data_frames"]])
  
  # Return
  return(data_frame_names)
  
}



#' Adds new data
#'
#' @param data_frame new data
#' @param name data name
#' @param dataset dataset name
#' name
#'
#' @return
#' @export
#'
#'
save_data_frame <- function(data_frame,
                            name,
                            dataset) {
  
  # Check input
  if (!hasArg(data_frame)) stop("No <data_frame> provided.", call. = FALSE)
  
  if (!hasArg(name)) stop("No <name> argument provided.", call. = FALSE)
  
  
  # Checks dataset
  dataset <- get_dataset(dataset)
  
  # Check data name
  if (name %in% get_data_frame_names(dataset)) 
    warning("Data frame <name> already exists and will be overwritten.", 
            call. = FALSE)
  
  
  # Check that a tibble is added
  if (!tibble::is_tibble(data_frame)) 
    stop("<data_frame> must be a tibble.", call. = FALSE)
  
  
  # Check column names
  if (colnames(data_frame)[1] != "observations")
    stop('First column must be named "observations".', call. = FALSE)
  
  # Check variables
  if (any(!colnames(data_frame)[-1] %in% get_variables(dataset = dataset))) 
    stop(paste0("Following variables do not exist in the dataset:", 
                paste(setdiff(colnames(data_frame)[-1], 
                              get_variables(dataset = dataset)), 
                      collapse = ", ")), call. = FALSE)
  
  # Check observations
  if (any(!data_frame[["observations"]] %in% 
          get_observations(dataset = dataset))) 
    stop(paste0("Following observations do not exist in the dataset:", 
                paste(setdiff(data_frame[["observations"]], 
                              get_observations(dataset = dataset)), 
                      collapse = ", ")), call. = FALSE)
  
  
  # Save data data
  Datasets[[dataset]][["Data_frames"]][[name]] <<- data_frame
  
  # Completion message
  message("The <data_frame> ", name, " was saved in the <dataset> ", 
          dataset, ".")
  # Return
  return(invisible(data_frame))
  
}


#' Assemble data frames from dataset and return as list
#'
#' @param which specific names of data type
#' @param variables selected variables
#' @param observations selected observations
#' @param dataset dataset name 
#' @param output.type data type (default = "tibble", "data.frame", "matrix")
#' @param match_variables Check if variables column names match between data 
#' frames 
#' @param match_observations Check if observations column matches between data 
#' frames 
#'
#' @return
#' @export
#'
#' @examples
get_data_frame_m <- function(which,
                             variables,
                             observations,
                             dataset, 
                             output.type = "tibble", 
                             match_variables = T, 
                             match_observations = T) {
  
  
  # Default data name
  if (!hasArg(which)) {
    stop("Specify the data frame name with <which>.", call. = FALSE)
  } else if (any(!is.character(which))) {
    stop("Provide a character string as data frame name with <which>.", 
         call. = FALSE)
  }
  
  # Save each dataa frame in list
  data_list <- list()
  
  # Save names
  if (length(names(which)) == length(which)) {
    which_names <- setNames(names(which), unname(which))
    which <- unname(which)
  } else {
    which <- unname(which)
    which_names <- setNames(unname(which), unname(which))
  }
  
  for (which in which) {
    
    # Checks correct name of dataset
    dataset <- get_dataset(dataset)
    
    # Check data frame 
    check_data_frame(which, dataset)
    
    # Assemble variables
    variables <- get_variables(variables = {{variables}},
                               dataset = dataset)
    
    # Assemble observations
    observations <- get_observations(observations = {{observations}},
                                     dataset = dataset)
    
    
    
    # Grab data
    data <- Datasets[[dataset]][["Data_frames"]][[which]]
    
    
    # Tibble 
    if (grepl(pattern = "tibble", x = output.type)) {
      
      data <- data %>%
        dplyr::filter(observations %in% !!observations) %>%
        dplyr::select(c(observations, dplyr::any_of(variables)))
      
      # Data frame
    } else if (grepl(pattern = "data.frame", x = output.type)) {
      
      data <- data %>%
        dplyr::filter(observations %in% !!observations) %>%
        dplyr::select(c(observations, dplyr::any_of(variables))) %>%
        tibble2data_frame()
      
      # Matrix
    } else if (grepl(pattern = "matrix", x = output.type)) {
      
      data <- data %>%
        dplyr::filter(observations %in% !!observations) %>%
        dplyr::select(c(observations, dplyr::any_of(variables))) %>%
        tibble2matrix()
      
    } else {
      
      stop("Data type <", output.type, 
           "> is not supported. Use <tibble>, <data.frame> ",
           "or <matrix> instead.", 
           call. = FALSE)
      
    }
    
    data_list[[which]] <- data
    
  }
  
  # Check data
  
  # Check matching observations 
  if (match_observations) {
    if (output.type == "tibble")
      list_observations <- lapply(data_list, \(x) x %>% 
                                    pull("observations"))
    else 
      list_observations <- lapply(data_list, rownames)
    
    for (i in seq_along(list_observations)[-1]) {
      if (any(list_observations[[1]] != list_observations[[i]]))
        stop("The observations do not match.")
    }
  }
  
  # Check matching observations 
  if (match_variables) {
    list_variables <- lapply(data_list, \(x) names(x))
    for (i in seq_along(list_variables)[-1]) {
      if (any(list_variables[[1]] != list_variables[[i]]))
        stop("The variables do not match.")
    }
  }
  
  # Rename data frames
  names(data_list) <- which_names[names(data_list)]
  
  # Return
  return(data_list)
  
}

