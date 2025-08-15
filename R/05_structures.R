#' NGLVieweR function which accepts protein accession IDs and downloads given 
#' protein structure from the Alphafold database
#'
#' @param id UniProt accession id 
#'
#' @returns
#' @export
#'
#' @examples
NGLVieweR_AFdb <- function(id) {
  
  file <- tempfile(fileext = ".pdb")
  
  download.file(paste0("https://alphafold.ebi.ac.uk/files/AF-", id, "-F1-model_v4.pdb"), 
                destfile = file)
  
  NGLVieweR(file)
  
}


