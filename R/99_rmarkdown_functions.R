#' Creates button to hide plots and tables
#' based on https://getbootstrap.com/docs/4.0/components/collapse/
#'
#' @param label button label
#'
#' @return
#' @export
#'
#'
button_begin <- function(label = "View/Hide") {
  
  # Buttons set up
  # Check
  if (!exists(".n.button")) .n.button <<- 0
  
  # Raise button counter
  .n.button <<- .n.button + 1
  
  # Paste names
  code <- paste0('<button class="btn btn-primary" type="button" data-toggle="collapse" data-target="#button',
                 .n.button,
                 '" aria-expanded="false" aria-controls="button', 
                 .n.button, 
                 '"> ',
                 label,
                 ' </button> <div id="button',
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





#' Title
#'
#' @param files 
#'
#' @return
#' @export
#'
#' @examples
load_m <- function(files) {
  
  if (any(!file.exists(files))) 
    stop("Following images do not exist: ", 
         paste(files[which(!file.exists(files))], collapse = ", "))
  
  image_merged <- new.env()
  load(files[1], envir = image_merged)
  
  list_images <- list()
  
  for (i in seq_along(files[-1])) {
    list_images[[i]] <- new.env()
    load(files[-1][i], envir = list_images[[i]])
  }
  
  
  # Add content from images one by one
  for (i in seq_along(list_images)) {
    
    # go through all object in environment
    for (j in objects(envir = list_images[[i]])) {
      
      # Add if new object
      if (!j %in% names(image_merged)) {
        image_merged[[j]] <- list_images[[i]][[j]]
        # overwrite if no list
      } else if (class(image_merged[[j]])[1] != "list") {
        image_merged[[j]] <- list_images[[i]][[j]]
        # if onject is list add entries recursively
      } else {
        # iterate through list entries
        # following code is the same as above
        for (k in names(list_images[[i]][[j]])) {
          
          # Level 2
          # Add if new object
          if (!k %in% names(image_merged[[j]])) {
            image_merged[[j]][[k]] <- list_images[[i]][[j]][[k]]
            # overwrite if no list
          } else if (class(image_merged[[j]][[k]])[1] != "list") {
            image_merged[[j]][[k]] <- list_images[[i]][[j]][[k]]
            # if object is list add entries recursively
          } else {
            
            # iterate through list entries
            # following code is the same as above
            for (l in names(list_images[[i]][[j]][[k]])) {
              
              # Level 3
              # Add if new object
              if (!l %in% names(image_merged[[j]][[k]])) {
                image_merged[[j]][[k]][[l]] <- list_images[[i]][[j]][[k]][[l]]
                # overwrite if no list
              } else if (class(image_merged[[j]][[k]][[l]])[1] != "list") {
                image_merged[[j]][[k]][[l]] <- list_images[[i]][[j]][[k]][[l]]
                # if onject is list add entries recursively
              }  else {
                image_merged[[j]][[k]][[l]] <- list_images[[i]][[j]][[k]][[l]]
                # the else statement could be extended to allow deeper lists here
              }
            }
          }
        }
      }
    }
  }
  
  for (i in names(image_merged)) {
    assign(i, image_merged[[i]], envir = globalenv())
  }
  
}


# 
#' Specific function for 00_Master script to fill Rmd template and add content
#'
#' @param name name of script and that should be replaced in the template file 
#' @param dir directory of the template 
#' @param dir.out output directory of the Rmd file 
#' @param template name of the Rmd template 
#' @param fill.pattern (optional) pattern to be replaced with existing Rmd 
#' script specified by name
#' @param output.name name of output Rmd file (defaults to gsub("00_template", 
#' name, template))
#'
#' @return
#' @export
#'
#' @examples
template2Rmd_fill <- function(name, 
                              dir = "Scripts", 
                              dir.out, 
                              template = "00_template.Rmd", 
                              fill.pattern = "# Content", 
                              output.name) {
  
  if (!hasArg(name)) 
    stop("Please provide a name.")
  
  if (!hasArg(output.name))
    output.name <- gsub("00_template", name, template)
  
  if (!hasArg(dir.out)) 
    dir.out <- dir
  
  
  text <- readLines(file.path(dir, template))
  
  text <- gsub("00_template", name, text)
  
  if (file.exists(file.path(dir, paste0(name, ".Rmd"))))
    text <- gsub(fill.pattern, 
                 paste(readLines(file.path(dir, paste0(name, ".Rmd"))), 
                       collapse = "\n"), 
                 text)
  
  cat(text, file = file.path(dir.out, output.name), sep = "\n")
  
}
