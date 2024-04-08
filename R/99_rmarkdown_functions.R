#' Sets up buttons for HTML document
#'
#' @return
#' @export
#'
#'
check_buttons <- function() {
  
  # Check
  if (!exists(".n.button")) .n.button <<- 0
  
}


#' Increases button count
#'
#' @return
#' @export
#'
#'
next_button <- function() {
  .n.button <<- .n.button + 1
}


#' Creates button to hide plots and tables
#'
#' @param label button label
#'
#' @return
#' @export
#'
#'
button_begin <- function(label = "View/Hide") {
  
  # Buttons set up
  check_buttons()
  
  # Raise button counter
  next_button()
  
  # Paste names
  code <- paste0('<button class="btn btn-primary" data-toggle="collapse" data-target="#',
                 .n.button,
                 '"> ',
                 label,
                 ' </button> <div id="',
                 .n.button,
                 '" class="collapse">  ')
  
  # return unevaluated code
  return(noquote(code))
  
}


#' Ends section hidden by button
#'
#' @return
#' @export
#'
#'
button_end <- function() {
  
  # Return
  return(noquote('</div>'))
  
}


#' Help make nice number in R Markdown
#'
#' @param x number
#' @param round should number be rounded
#' @param digits round to number of digits after comma
#' @param format number format (see formatC())
#' @param big.mark separator between 3 digits; remove with ""
#' @param sci.digits digits after decimal point for scientific notation
#' (format = "e)
#' @param output type of output vector
#'
#' @return
#' @export
#'
#'
nice_number <- function(x,
                        round = T,
                        digits = 0,
                        format = "d",
                        big.mark = ",",
                        sci.digits,
                        output = "numeric") {
  
  if (round) x <- round(x, digits = digits)
  
  if (big.mark != "" & !hasArg(sci.digits)) {
    x <- formatC(x, format = format, big.mark = big.mark)
    output <- "character"
  }
  
  else if (big.mark != "") x <- formatC(x,
                                        format = format,
                                        big.mark = big.mark,
                                        digits = sci.digits)
  
  if (output == "numeric") x <- as.numeric(x)
  
  # Return
  return(x)
  
}
