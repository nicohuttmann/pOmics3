#' Prints vectors to console in as character vector
#'
#' @param ... vector/s
#'
#' @return
#' @export
#'
#'
.cat_character <- function(...) {
  
  cat(paste0('c("',
             paste(..., collapse = '",\n\t"'),
             '")'))
  
}


#' Prints vectors to console in as character vector
#'
#' @param ... vector/s
#'
#' @return
#' @export
#'
#'
.cat_character_named <- function(...) {
  
  n <- paste0(names(...), '" = "', ..., '"')
  
  cat(paste0('c("', paste(n, collapse = ',\n\t"'), ')'))
  
}


#' Prints vectors to console in as character vector
#'
#' @param ... vector/s
#'
#' @return
#' @export
#'
#'
.cat_numeric <- function(...) {
  
  cat(paste0('c(', paste(..., collapse = ',\n\t'), ')'))
  
}


#' Substitutes pattern multiple times and prints output as code
#'
#' @param x character string containing pattern to be substituted
#' @param pattern character pattern to be substituted
#' @param replacement character vector of strings to use for substitution
#' @param sep separator string between n outputs
#'
#' @return
#' @export
#'
#' @examples
.cat_gsub_n <- function(x, pattern, replacement, sep = "\n\n") {
  
  for (i in replacement) {
    
    x %>% 
      gsub(pattern, i, .) %>% 
      cat(sep)
    
  }
  
}


#' Prints function snippet to console and clipboard
#'
#' @param FUN function
#' @param add.equal.sign add equal sign to add arguments
#' @param print print function snippet to console
#' @param copy2clipboard copy function snippet to clipboard
#'
#' @return
#' @export
#'
#'
.cat_function <- function(FUN, add.equal.sign = T) {
  
  #
  if (!hasArg(FUN) || !is.function(FUN)) return(FALSE)
  
  
  
  FUN.str <- deparse(args(FUN))
  
  # Remove last element of vector
  FUN.str <- FUN.str[FUN.str != "NULL"]
  
  
  # Collapse vector to one string
  FUN.str <-  paste(FUN.str, collapse = "")
  
  # Catch function name
  name <- deparse(substitute(FUN))
  
  FUN.str <- gsub(pattern = "function ",
                  replacement = name,
                  x = FUN.str)
  
  # Remove double spaces
  while (nchar(FUN.str) != nchar(gsub(pattern = "  ",
                                      replacement = " ",
                                      x = FUN.str))) {
    FUN.str <- gsub(pattern = "  ",
                    replacement = " ",
                    x = FUN.str)
  }
  
  
  # Separate arguments
  FUN.str <- unlist(strsplit(FUN.str, split = ", "))
  
  
  # Add = sign
  if (add.equal.sign) {
    
    for (i in setdiff(which(!grepl("=", FUN.str)), length(FUN.str))) {
      
      FUN.str[i] <- paste0(FUN.str[i], " = ")
      
    }
    
    # Modify last argument if no default is given
    if (!grepl("=", FUN.str[length(FUN.str)])) {
      FUN.str[length(FUN.str)] <-
        paste0(substring(FUN.str[length(FUN.str)],
                         1,
                         nchar(FUN.str[length(FUN.str)]) - 2),
               " = ",
               substring(FUN.str[length(FUN.str)],
                         nchar(FUN.str[length(FUN.str)]) - 1))
    }
    
  }
  
  # Combine arguments with linebreaks and tabs
  FUN.str <- paste(FUN.str, collapse = ", \n\t")
  
  # print to console
  cat(FUN.str)
  
  # Return
  return(invisible(FUN.str))
  
}


#' Guesses observations names from raw_dataset
#'
#' @param x data frame
#' @param pattern separation pattern of column names
#' @param sample.as.suffix
#' @param to.include observations that must be included; helps to identify
#' correct observations
#'
#' @return
#' @export
#'
#'
.identify_observations <- function(x,
                                  pattern = ".",
                                  sample.as.suffix = T,
                                  to.include = NULL) {
  
  if (sample.as.suffix) {
    fix <- substring(text = colnames(x), 1, str_locate_last(colnames(x), pattern = pattern) - 1)
  } else {
    fix <- substring(text = colnames(x), str_locate_last(colnames(x), pattern = pattern) + 1)
  }
  
  
  tab <- sort(table(fix), decreasing = TRUE)
  
  tab <- tab[nchar(names(tab)) >= 3]
  
  # Important: Exclusion list for potential sample names
  tab <- tab[!names(tab) %in% c("Count", "IDs", "acid", "window", "position",
                                "names", "[%]", "Peptide.counts")]
  
  # Consider predefined observations
  if(is.null(to.include)) {
    # Choose observations
    observations <- colnames(x)[grepl(names(tab)[1], colnames(x))] %>%
      substring(first = nchar(names(tab)[1]) + 2)
  } else {
    observations <- colnames(x)[grepl(names(tab)[1], colnames(x))] %>%
      substring(first = nchar(names(tab)[1]) + 2)
    while (!all(to.include) %in% observations) {
      if (length(tab) == 0) {
        stop(paste0(
          "Given observations ",
          paste(to.include, collapse = ", "),
          " could not all be identified in the data frame. Check your input or the algorithm."
        ))
      }
      tab <- tab[-1]
      observations <- colnames(x)[grepl(names(tab)[1], colnames(x))] %>%
        substring(first = nchar(names(tab)[1]) + 2)
    }
  }
  
  return(observations)
  
}



#' Title
#'
#' @param raw.data 
#' @param variable.identifiers 
#' @param variables.data 
#' @param observations 
#' @param data.frames 
#'
#' @return
#' @export
#'
#' @examples
.cat_import2new_dataset <- function(raw.data, 
                                    variable.identifiers = "Protein.ID", 
                                    variables.data = c("Entry.Name", 
                                                       "Gene", 
                                                       "Protein.Description", 
                                                       "Protein", 
                                                       "Unique.Peptides"), 
                                    observations, 
                                    data.frames = "channel") {
  
  
  if (!hasArg(observations.names)) observations.names <- observations
  
  # Print output
  cat(paste0(
    'Datasets[["', raw.data, '"]] <- \n\t', 
    "import2new_dataset(raw.data = Info$Imports$", raw.data, ",\n\t", 
    "variable.identifiers = ", variable.identifiers, ",\n\t", 
    
    'variables.data = c("', 
    paste(variables.data, collapse = '",\n\t"'),
    '"),\n\t', 
    
    'observations = c("', 
    paste(
      paste0(names(observations), '" = "', observations, '"'), 
      collapse = ',\n\t"'), 
    '),\n\t', 
    
    'data.frames = c("', 
    paste(data.frames, collapse = '",\n\t"'),
    '"))\n\t'
  ))
  
}


#' Define multiple mutate opereations by a formula and a corresponding data frame
#'
#' @param x tibble with column names to be replace by each row 
#' @param formula formula as a string containing the column names to be replaced
#' @param copy2clipboard copy the code to the clipboard
#'
#' @return
#' @export
#'
#' 
.cat_mutate_from_dataframe <- function(x, 
                                       formula = "", 
                                       copy2clipboard = T) {
  output <- map(1:nrow(x), \(i) 
                x %>% 
                  filter(row_number() == i) %>% 
                  c() %>% 
                  unlist() %>% 
                  str_replace_all(formula, .)) %>% 
    unlist() %>% 
    paste(., collapse = ',\n\t') %>% 
    paste0('mutate(',
           .,
           ')')
  
  if (copy2clipboard) 
    cat(output, 
        file = "clipboard")
  
  cat(output)
  
  return(invisible(output))
  
}

