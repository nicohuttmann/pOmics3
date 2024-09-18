# x must be a tibble with the columns Protein.Group and Stripped.Sequence
# fasta is the location of the fasta file in the folder
#' Title
#'
#' @param x tibble 
#' @param fasta location of a fasta file
#' @param identifier column name of protein/groups id
#' @param sequence column name of peptide sequences
#'
#' @return
#' @export
#'
#' 
mutate_sequence_position <- function(x, 
                                     fasta, 
                                     identifier = "Protein.Group", 
                                     sequence = "Stripped.Sequence") {
  
  if (!hasArg(x)) 
    stop("Please specify a data frame or a tibble of the DIA-NN precursors.")
  
  if (!hasArg(fasta)) stop("No fasta file specified.")
  fasta <- Biostrings::readAAStringSet(fasta)
  fasta <- as.character(fasta)
  names(fasta) <- unlist(lapply(strsplit(x = names(fasta), "\\|"), \(x) x[2]))
  
  x_pos <- x %>% 
    mutate(Sequence = fasta[strsplit_keep_first(Protein.Group, ";")], 
           Start = str_locate(Sequence, Stripped.Sequence)[, "start"], 
           End = str_locate(Sequence, Stripped.Sequence)[, "end"], 
           Length = nchar(Stripped.Sequence))
  
  return(x_pos)
  
}

