#' Transforms tibble to matrix
#'
#' @param data tibble
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
tibble2matrix <- function(data) {
  
  # Check input
  if (!tibble::is_tibble(data)) stop("The argument <data> is not a tibble.", 
                                       call. = FALSE)
  
  # Transform to tibble
  data <- data %>%
    dplyr::rename(rowname = 1) %>% 
    tibble::column_to_rownames() %>%
    as.matrix()

  # Transform and return
  return(data)
  
}


#' Transforms tibble to data frame
#'
#' @param data tibble
#'
#' @return
#' @export
#'
tibble2data_frame <- function(data) {
  
  # Check input
  if (!tibble::is_tibble(data)) stop("Argument <data> is not a tibble.", 
                                       call. = FALSE)
  
  # Transform to data frame
  data <- data %>% 
    dplyr::rename(rowname = 1) %>% 
    tibble::column_to_rownames()
  
  # Transform and return
  return(data)
  
}


#' Transforms matrix to tibble and adds row names as first column 
#'
#' @param data matrix with row names
#' @param to.row.names name for row names column
#'
#' @return
#' @export
#'
#'
matrix2tibble <- function(data, to.row.names = "rows") {
  
  # Check input
  if (!is.matrix(data)) stop("Argument <data> is not a matrix.", 
                             call. = FALSE)
  
  # Transform to tibble
  data <- data %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = to.row.names) %>%
    tibble::as_tibble()
  
  # Transform and return
  return(data)
  
}


#' Transforms data frames to tibble and adds column for row names
#'
#' @param data data frame with row names
#' @param to.row.names name for row names column
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
data_frame2tibble <- function(data, to.row.names = "rows") {
  
  # Transform data.frame
  data <- data %>%
    tibble::rownames_to_column(var = to.row.names) %>%
    tibble::as_tibble()
  
  # Transform and return
  return(data)
  
}


#' Transforms any data type to a tibble
#'
#' @param data supported data types: matrix, data.frame
#' @param to.row.names name for row names vector
#'
#' @return
#' @export
#'
#'
data2tibble <- function(data, to.row.names = "rows") {
  
  # Already a tibble
  if (tibble::is_tibble(data)) {
    return(data)
    
    # Matrix
  } else if (is.matrix(data)) {
    
    data <- matrix2tibble(data, to.row.names)
    
    # Data frame
  } else if (is.data.frame(data)) {
    
    data <- data_frame2tibble(data, to.row.names)
    
    # Other data types
  } else {
    stop("Data type not a data.frame or a matrix.", call. = FALSE)
  }
  
  # Return
  return(data)
  
}


#' Transposes tibble and uses first column as column names
#'
#' @param data tibble
#' @param to.row.names row names column after transposing (if first column is 
#'  "variables" or "observations", it will automatically assume the other)
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
transpose_tibble <- function(data, to.row.names) {
  
  # Use rows as new column name
  if (!hasArg(to.row.names)) {
    if (colnames(data)[1] == "observations") {
      to.row.names <- "variables"
    } else if (colnames(data)[1] == "variables") {
      to.row.names <- "observations"
    } else {
      to.row.names <- "rows"
    }
  }
  
  # Transpose
  data_t <- data %>%
    tibble2matrix() %>%
    t() %>%
    matrix2tibble(to.row.names = to.row.names)
  
  # Return transposed tibble
  return(data_t)
  
}


#' Transforms a list to tibble logical indication for variables
#'
#' @param x list
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
list2logical_df <- function(x, identifier = "variables") {
  
  #
  df <- tibble::tibble(!!identifier := unique(unlist(x)))
  
  # Add logical columns
  for (column in names(x)) {
    df <- dplyr::mutate(df, !!column := variables %in% x[[column]])
  }
  
  # Return
  return(df)
  
}

