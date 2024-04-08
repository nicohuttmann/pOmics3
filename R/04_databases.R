#' Title
#' 
#' Instruction by Henrik Hammaren
#' To find names of columns
#' go to
#' https://www.uniprot.org/uniprotkb?facets=reviewed%3Atrue&query=%28proteome%3AUP000005640%29
#' Click "Download"
#' Format "TSV"
#' Compressed "No"
#' Choose columns as you want
#' Bottom of page: "Generate URL for API"
#' Voila.
#'
#' @param url url decribing the anticipated columns from UniProt REST API
#'
#' @return
#' @export
#'
#' @examples
db_download_UniProt <- function(url = "https://rest.uniprot.org/uniprotkb/stream?fields=accession%2Creviewed%2Cid%2Cprotein_name%2Cgene_names%2Corganism_name%2Clength%2Cgo%2Cgo_p%2Cgo_c%2Cgo_f%2Cxref_corum%2Cxref_string&format=tsv&query=%28%28taxonomy_id%3A10090%29+AND+%28reviewed%3Atrue%29%29") {
  
  db <- vroom::vroom(url)
  
  return(db)
  
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

