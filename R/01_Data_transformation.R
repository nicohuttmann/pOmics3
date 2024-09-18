#' Applies limma::normalizeBetweenArrays to an matrix but calculates 
#' normalization factors only based on a subset of the proteins
#'
#' @param object 
#' @param method 
#' @param targets 
#' @param cyclic.method 
#' @param reference_proteins 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
normalizeBetweenArrays_subset <- function(object, 
                                          method, 
                                          targets, 
                                          cyclic.method, 
                                          reference_proteins, 
                                          ...) {
  
  if (!hasArg(reference_proteins)) {
    reference_proteins <- rownames(object)
    warning("No <reference_proteins> supplied, using all proteins for normalization.")
  }
  
  reference <- intersect(reference_proteins, rownames(object))
  
  if (length(reference) == 0) 
    stop("No <reference_proteins> were found in the matrix.")
  
  else if (length(reference_proteins) != length(reference)) 
    warning("Not all <reference_proteins> were present in the matrix.")
  
  object_subset <- object[reference, ]
  
  object_subset_norm <- limma::normalizeBetweenArrays(object = object_subset, 
                                                      method = method, 
                                                      targets = targets, 
                                                      cyclic.method = cyclic.method, 
                                                      ...) 
  
  norm_factors <- (object_subset_norm / object_subset)[1, ]
  
  object_norm <- t(apply(object, 1, \(x) x * norm_factors))
  
  return(object_norm)
  
}


