#' Title
#'
#' @param data_UniProt_GO UniProt data for GO annotations as downloaded by 
#' get_UniProt_data(ids, fields = UniProt_fields()$`Gene Ontology (GO)`)
#' @param entry_col protein identifier column 
#' @param GO_col name of column containing GO annotations to be extracted 
#'
#' @returns
#' @export
#'
#' @examples
extract_UniProt_GO2TERM2GENE <- function(data_UniProt_GO, 
                                         entry_col = "Entry", 
                                         GO_col = c("BP", "CC", "MF", 
                                                    "all", 
                                                    "ids")) {
  
  # Check if input for GO_col was changed 
  if (length(GO_col) > 1) {
    warning("Please only specify one column.")
    GO_col <- GO_col[1]
  }
  
  # Substitute known names 
  if (GO_col %in% c("BP", "CC", "MF", "all", "ids")) {
    GO_col <- c(BP = "Gene Ontology (biological process)",
                CC = "Gene Ontology (cellular component)",
                all = "Gene Ontology (GO)",
                MF = "Gene Ontology (molecular function)",
                ids = "Gene Ontology IDs")[GO_col] %>% 
      unname()
  }
  
  
  data_GO <- data_UniProt_GO %>% 
    dplyr::select(all_of(c(GO_col, entry_col)))%>% 
    tidyr::separate_longer_delim(cols = GO_col, delim = "; ") %>% 
    dplyr::filter(!!rlang::sym(GO_col) != "")
  
  return(data_GO)
  
}


#' Title
#'
#' @param data_UniProt_GO UniProt data for GO annotations as downloaded by 
#' get_UniProt_data(ids, fields = UniProt_fields()$`Gene Ontology (GO)`)
#' @param entry_col protein identifier column 
#' @param GO_col name of column containing GO annotations to be extracted 
#'
#' @returns
#' @export
#'
#' @examples
extract_UniProt_GO2list <- function(data_UniProt_GO, 
                                    entry_col = "Entry", 
                                    GO_col = c("BP", "CC", "MF", 
                                               "all", 
                                               "ids")) {
  
  # Check if input for GO_col was changed 
  if (length(GO_col) > 1) {
    warning("Please only specify one column.")
    GO_col <- GO_col[1]
  }
  
  # Substitute known names 
  if (GO_col %in% c("BP", "CC", "MF", "all", "ids")) {
    GO_col <- c(BP = "Gene Ontology (biological process)",
                CC = "Gene Ontology (cellular component)",
                all = "Gene Ontology (GO)",
                MF = "Gene Ontology (molecular function)",
                ids = "Gene Ontology IDs")[GO_col] %>% 
      unname()
  }
  
  
  # Extract a Term2Gene data frame 
  data_GO <- data_UniProt_GO %>% 
    dplyr::select(all_of(c(entry_col, GO_col)))%>% 
    tidyr::separate_longer_delim(cols = GO_col, delim = "; ") %>% 
    dplyr::filter(!!rlang::sym(GO_col) != "")
  
  # Make list from T2G data frame 
  list_GO <- purrr::map(dplyr::pull(data_GO, GO_col, GO_col) %>% 
                          unique() %>% 
                          setNames(., .), 
                        \(x)  data_GO %>% 
                          dplyr::filter(!!dplyr::sym(GO_col) == x) %>% 
                          dplyr::pull(!!entry_col))
  
  return(list_GO)
  
}


#' Title
#'
#' @param goids 
#' @param ontology 
#'
#' @returns
#' @export
#'
#' @examples
GO_add_ancestors <- function(goids, ontology = "CC") {
  
  list_ancestors <- 
    switch(ontology, 
           "CC" = as.list(GO.db::GOCCANCESTOR), 
           "BP" = as.list(GO.db::GOBPANCESTOR), 
           "MF" = as.list(GO.db::GOMFANCESTOR), 
           stop('<ontology> must be one of "CC", "BP" or "MF."'))
  
  
  if (is.list(goids)) {
    
    goids_expanded <- purrr::map(goids, \(x) list_ancestors[x] %>% 
                                   unlist() %>% 
                                   unname() %>% 
                                   base::setdiff("all") %>% 
                                   c(x, .))
    
  } else if (is.vector(goids)) {
    
    goids_expanded <- list_ancestors[goids] %>% 
      unlist() %>% 
      unname() %>% 
      base::setdiff("all") %>% 
      c(goids, .)
    
  } else {
    stop("Please provide the GO Ids as a vector or list of vectors.")
  }
  
  return(goids_expanded)
  
}



GO_filter_ontology <- function(goids, ontology = "CC") {
  
  list_terms <- as.list(GO.db::GOTERM)
  
  if (is.list(goids)) {
    
    goids_filtered <- 
      map(goids, \(y) {
        
        y[purrr::map_lgl(y, 
                         \(x) 
                         tryCatch(AnnotationDbi::Ontology(list_terms[[x]]) == 
                                    ontology, 
                                  error = function(e) F))]
        
      })
    
    
  } else if (is.vector(goids)) {
    
    goids_in_ontology <- 
      map_lgl(goids, 
              \(x) tryCatch(AnnotationDbi::Ontology(list_terms[[x]]) == 
                              ontology, 
                            error = function(e) F))
    
    goids_filtered <- goids[goids_in_ontology]
    
  } else {
    stop("Please provide the GO Ids as a vector or list of vectors.")
  }
  
  return(goids_filtered)
  
}



GO_append_description <- function(goids) {
  
  list_terms <- as.list(GO.db::GOTERM)
  
  if (is.list(goids)) {
    
    goids_appended <- 
      purrr::map(goids, \(y) 
                 purrr::map_chr(y, 
                                \(x) tryCatch(
                                  paste0(AnnotationDbi::Term(list_terms[[x]]), 
                                         " [", x, "]"), 
                                  error = function(e) x)))
    
  } else if (is.vector(goids)) {
    
    goids_appended <- 
      purrr::map_chr(goids, 
                     \(x) tryCatch(paste0(AnnotationDbi::Term(list_terms[[x]]),
                                          " [", x, "]"), 
                                   error = function(e) x))
    
  } else {
    stop("Please provide the GO Ids as a vector or list of vectors.")
  }
  
  return(goids_appended)
  
}
