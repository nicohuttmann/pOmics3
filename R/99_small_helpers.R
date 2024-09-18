#' F(actors)U(nique) 
#'
#' @param x a vector
#'
#' @return
#' @export
#'
#' 
fu <- function (x) {
  factor(x = x, levels = unique(x))
}

