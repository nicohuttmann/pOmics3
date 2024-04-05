#' Imports any file type using the file extension and returns list
#'
#' @param files file paths
#' @param dir directory to import files from
#' @param ext specific file extensions to imports
#' @param silent suppresses messages
#' @param ... additional arguments to vroom function
#'
#' @importFrom magrittr %>% 
#'
#' @return
#' @export
#'
#'
import_files <- function(files, silent = F, ...) {
  
  # Select files if no path given
  if (!hasArg(files) & !hasArg(dir)) {
    
    stop("File names(s) have to be specified.")
    
  } 
  
  
  # Create list to store imported files
  list.import <- list()
  
  
  
  # Import all files
  for (i in seq_along(files)) {
    
    # Import data file
    if (silent) data <- suppressWarnings(
      suppressMessages(
        vroom::vroom(file = files[i], ...)))
    
    else data <- suppressWarnings(vroom::vroom(file = files[i], ...))
    
    
    # Rename columns to avoid spaces
    names(data) <- gsub(pattern = " ", replacement = ".", names(data))
    names(data) <- gsub(pattern = "\\.+", replacement = "\\.", names(data))
    
    
    
    # Add file to list
    list.import[[length(list.import) + 1]] <- tibble::as_tibble(data)
    
    
  }
  
  # Modify file names
  if (!is.null(names(files))) {
    if (any(duplicated(names(files)))) {
      stop("Names of the files argument vector must be unique.")
    } else {
      file.names <- names(files)
    }
  } else {
    file.names <- files %>%
      strsplit(split = "/") %>% 
      lapply(function(x) x[length(x)]) %>% 
      unlist() %>%
      tools::file_path_sans_ext()
    # Check if names are unique
    if (any(duplicated(file.names))) {
      stop("File names are not unique. ", 
           "Specify the names for by naming the files vector.", 
           call. = FALSE)
    }
  }
  
  
  # Add names
  names(list.import) <- file.names
  
  # Add imported files to .import list
  .save_import(list.import)
  
  # Return list or one data frame
  return(invisible(list.import))
  
}


#' Saves imported files to .imports list
#'
#' @param import list of imported files
#'
#' @return
#' @export
#'
#'
.save_import <- function(import) {
  
  # Check for imports list
  if (exists("Info")) {
    if("Imports" %in% names(Info)) {
    } else {
      stop("The <Imports> entry does not exist in the <Info> list. Create ", 
           "it with: 
           Info$Import <- list()", 
           call. = FALSE)
    }
  } else {
    stop("The <Info> list does not exist. Create it with:
         Info <- list(Imports = list())", 
         call. = FALSE)
  }
  
  # All imported files in list
  for (i in seq_along(import)) {
    
    Info[["Imports"]][[names(import)[i]]] <<- import[[i]]
    
  }
  
}