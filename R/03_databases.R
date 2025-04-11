#' Download UniProt data for given protein accessions and data fields 
#' (see available fields with UniProt_fields())
#'
#' @param accession vector of UniProt accessions 
#' @param fields UniProt data fields to query
#' @param max.query maximum number of accessions to query at once; if the 
#' the number exceeds max.query, the query is split up in multiple parts 
#'
#' @returns
#' @export
#'
#' @examples
get_UniProt_data <- function(accession, 
                             fields = c("accession", 
                                        "gene_names", 
                                        "organism_name"), 
                             max.query = 1000) {
  
  # Add accession as field
  if (!"accession" %in% fields) fields <- c("accession", fields)
  
  # Check accession for ;
  if (any(stringr::str_detect(accession, ";"))) 
    stop("There are accessions contanining a semicolon (;);, please remove or correct protein groups with multiple Ids.")
  
  # Only query unique accessions 
  accession_query <- unique(accession)
  
  
  # Formulate query/ies and download data 
  if (length(accession_query) > max.query) {
    
    l <- length(accession_query)
    from <- seq(1, l, 1000)
    to <- c(seq(1, l, 1000)[-1] - 1, l)
    
    data_download <- purrr::map2(from, to, 
                                 \(from, to) get_UniProt_data(accession_query[from:to], 
                                                              fields = fields, 
                                                              max.query = max.query)) %>% 
      bind_rows()
    
  } else {
    
    query_url <- paste0("https://rest.uniprot.org/uniprotkb/stream?", 
                        "format=tsv", 
                        "&fields=", 
                        paste(fields, collapse = "%2C"), 
                        "&query=", 
                        paste(
                          paste0("accession%3A", accession_query), 
                          collapse = "+OR+"))
    
    data_download <- vroom::vroom(query_url, 
                                  delim = "\t", 
                                  col_types = readr::cols())
    
  }
  
  # Merge given accessions and downloaded data 
  data_output <- dplyr::left_join(tibble::tibble(Entry = accession), 
                                  data_download, 
                                  by = "Entry")
  
  # Return tibble with accessions as Entry and data columns 
  return(data_output)
  
}


#' Download UniProt data for given protein accessions, taxonomy identifiers and 
#' data fields (faster for 1000s of proteins; see available fields with 
#' UniProt_fields())
#'
#' @param accession 
#' @param fields 
#' @param taxon_id 
#'
#' @returns
#' @export
#'
#' @examples
get_UniProt_data_1o <- function(accession, 
                                fields = c("accession", 
                                           "gene_names", 
                                           "organism_name"), 
                                taxon_id = c(human = 9606, 
                                             mouse = 10900, 
                                             E.coliK12 = 83333)) {
  
  # Check organism identifier
  if (length(taxon_id) > 1) 
    warning("More than one taxon_id provided, only using the first one.")
  
  # Add accession as field
  if (!"accession" %in% fields) fields <- c("accession", fields)
  
  
  # Formulate query and download data 
  query_url <- paste0("https://rest.uniprot.org/uniprotkb/stream?", 
                      "format=tsv", 
                      "&fields=", 
                      paste(fields, collapse = "%2C"), 
                      "&query=%28model_organism%3A", 
                      taxon_id[1], "%29")
  
  data_download <- vroom::vroom(query_url, 
                                delim = "\t", 
                                col_types = readr::cols())
  
  
  # Merge given accessions and downloaded data 
  if (hasArg(accession)) {
    data_output <- dplyr::left_join(tibble::tibble(Entry = accession), 
                                  data_download, 
                                  by = "Entry")
  } else {
    data_output <- data_download
  }
  
  
  # Return tibble with accessions as Entry and data columns 
  return(data_output)
  
}


#' Wrapper function to download data from Reactome pathways
#'
#' @param url 
#' @param col_names 
#'
#' @return
#' @export
#'
#' @examples
download_Reactome <- function(
    url = "https://reactome.org/download/current/UniProt2Reactome.txt", 
    col_names = c("UniProt",
                  "Reactome_Pathway_Stable_identifier",
                  "URL",
                  "Name",
                  "Evidence_Code",
                  "Species")) {
  
  db <- vroom::vroom(file = url, col_names = col_names)
  
  return(db)
  
}


#' Wrapper function to download STRING databases
#'
#' @param url 
#'
#' @return
#' @export
#'
#' @examples
db_download_STRING <- function(type = c("physical_links", 
                                        "physical_links_detailed", 
                                        "physical_links_full", 
                                        "links", 
                                        "links_detailed", 
                                        "links_full"), 
                               taxid = 9606) {
  
  urltype <- c(physical_links = "protein.physical.links.v12.0", 
               physical_links_detailed = "protein.physical.links.detailed.v12.0", 
               physical_links_full = "protein.physical.links.full.v12.0", 
               links = "protein.links.v12.0", 
               links_detailed = "protein.links.detailed.v12.0", 
               links_full = "protein.links.full.v12.0")
  
  
  url <- paste0("https://stringdb-downloads.org/download/", 
                urltype[type[1]], "/", 
                taxid, ".", 
                urltype[type[1]], ".txt.gz")
  
  db_STRING <- vroom::vroom(url)
  
  return(db_STRING)
  
}


#' Wrapper function to download 'All levels' data from Reactome pathways
#'
#' @param url 
#' @param col_names 
#'
#' @return
#' @export
#'
#' @examples
download_Reactome_All_Levels <- function(
    url = "https://reactome.org/download/current/UniProt2Reactome_All_Levels.txt", 
    col_names = c("UniProt",
                  "Reactome_Pathway_Stable_identifier",
                  "URL",
                  "Name",
                  "Evidence_Code",
                  "Species")) {
  
  db <- vroom::vroom(file = url, col_names = col_names)
  
  return(db)
  
}


#' Wrapper function to download 'Reactions' data from Reactome pathways
#'
#' @param url 
#' @param col_names 
#'
#' @return
#' @export
#'
#' @examples
download_ReactomeReactions <- function(
    url = "https://reactome.org/download/current/UniProt2ReactomeReactions.txt", 
    col_names = c("UniProt",
                  "Reactome_Pathway_Stable_identifier",
                  "URL",
                  "Name",
                  "Evidence_Code",
                  "Species")) {
  
  db <- vroom::vroom(file = url, col_names = col_names)
  
  return(db)
  
}


db_download_ComplexPortal <- function(taxID) {
  
  url <-  paste0(
    "http://ftp.ebi.ac.uk/pub/databases/intact/complex/current/complextab/", 
    taxid, 
    ".tsv")
  
  db <- vroom::vroom(url)
  
  return(db)
  
}


#' Prepares tables to gene set list for other functions, e.g. fgsea
#'
#' @param db_table 
#' @param set 
#' @param proteins 
#'
#' @return
#' @export
#'
#' @examples
db_table2sets <- function(db_table, 
                          set = "Name", 
                          proteins = "UniProt") {
  
  sets <- purrr::map(dplyr::pull(db_table, set, set) %>% 
                       unique() %>% 
                       setNames(., .), 
                     ~ db_table %>% 
                       dplyr::filter(!!dplyr::sym(set) == .x) %>% 
                       dplyr::pull(!!proteins))
  
  return(sets)
  
}


#' Prepares a GO gene set from a downloaded UniProt table for other functions, 
#' e.g. fgsea
#'
#' @param db_UniProt 
#' @param ontology 
#'
#' @return
#' @export
#'
#' @examples
db_UniProt2GO <- function(db_UniProt, 
                          ontology = c("all", "BP", "CC", "MF")) {
  
  if (!hasArg(db_UniProt)) stop("Please provide a database derived by UniProt.")
  
  if (!ontology[1] %in% c("all", "BP", "CC", "MF")) 
    stop("Please use one of the given options for <ontology>.")
  
  ontology <- c(all = "Gene Ontology (GO)", 
                BP = "Gene Ontology (biological process)", 
                CC = "Gene Ontology (cellular component)", 
                MF = "Gene Ontology (molecular function)")[ontology]
  
  db_preGO <- db_UniProt %>% 
    dplyr::filter(!is.na(!!dplyr::sym(ontology))) %>% 
    tidyr::separate_longer_delim(!!sym(ontology), "; ")
  
  db_GO <- db_table2sets(db_preGO, 
                         set = ontology, 
                         proteins = "Entry")
  
  return(db_GO)
  
}


#' Translate STRING IDs in downloaded STRING database to UniProt Accessions 
#'
#' @param db_STRING 
#' @param db_UniProt 
#'
#' @return
#' @export
#'
#' @examples
db_STRING2UniProt <- function(db_STRING, 
                              db_UniProt) {
  
  STRING2UniProt <- db_UniProt %>% 
    dplyr::mutate(STRING = gsub(";", "", STRING)) %>% 
    dplyr::pull(Entry, STRING)
  
  db_STRING_UniProt <- db_STRING %>% 
    dplyr::mutate(protein1 = ifelse(!is.na(STRING2UniProt[protein1]), 
                                    STRING2UniProt[protein1], 
                                    protein1)) %>% 
    dplyr::mutate(protein2 = ifelse(!is.na(STRING2UniProt[protein2]), 
                                    STRING2UniProt[protein2], 
                                    protein2))
  
  return(db_STRING_UniProt)
  
}

