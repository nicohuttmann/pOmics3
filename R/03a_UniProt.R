#' Return list of vectors of UniProt data fields 
#'
#' @returns
#' @export
#'
#' @examples
UniProt_fields <- function() {
  
  UniProt_fields <- list(
    `Names & Taxonomy` = c("Entry Name" = "id",
                           "Gene Names" = "gene_names",
                           "Gene Names (ordered locus)" = "gene_oln",
                           "Gene Names (ORF)" = "gene_orf",
                           "Gene Names (primary)" = "gene_primary",
                           "Gene Names (synonym)" = "gene_synonym",
                           "Organism" = "organism_name",
                           "Organism (ID)" = "organism_id",
                           "Protein names" = "protein_name",
                           "Proteomes" = "xref_proteomes",
                           "Taxonomic lineage" = "lineage",
                           "Taxonomic lineage (Ids)" = "lineage_ids",
                           "Virus hosts" = "virus_hosts"), 
    Sequences = c("Alternative products (isoforms)" = "cc_alternative_products",
                  "Alternative sequence" = "ft_var_seq",
                  "Erroneous gene model prediction" = "error_gmodel_pred",
                  "Fragment" = "fragment",
                  "Gene encoded by" = "organelle",
                  "Length" = "length",
                  "Mass" = "mass",
                  "Mass spectrometry" = "cc_mass_spectrometry",
                  "Natural variant" = "ft_variant",
                  "Non-adjacent residues" = "ft_non_cons",
                  "Non-standard residue" = "ft_non_std",
                  "Non-terminal residue" = "ft_non_ter",
                  "Polymorphism" = "cc_polymorphism",
                  "RNA Editing" = "cc_rna_editing",
                  "Sequence" = "sequence",
                  "Sequence caution" = "cc_sequence_caution",
                  "Sequence conflict" = "ft_conflict",
                  "Sequence uncertainty" = "ft_unsure",
                  "Sequence version" = "sequence_version"), 
    Function = c("Absorption" = "absorption",
                 "Active site" = "ft_act_site",
                 "Binding site" = "ft_binding",
                 "Catalytic activity" = "cc_catalytic_activity",
                 "Cofactor" = "cc_cofactor",
                 "DNA binding" = "ft_dna_bind",
                 "EC number" = "ec",
                 "Activity regulation" = "cc_activity_regulation",
                 "Function [CC]" = "cc_function",
                 "Kinetics" = "kinetics",
                 "Pathway" = "cc_pathway",
                 "pH dependence" = "ph_dependence",
                 "Redox potential" = "redox_potential",
                 "Rhea ID" = "rhea",
                 "Site" = "ft_site",
                 "Temperature dependence" = "temp_dependence"), 
    Miscellaneous = c("Annotation" = "annotation_score",
                      "Caution" = "cc_caution",
                      "Keywords" = "keyword",
                      "Keyword ID" = "keywordid",
                      "Miscellaneous [CC]" = "cc_miscellaneous",
                      "Protein existence" = "protein_existence",
                      "Reviewed" = "reviewed",
                      "Tools" = "tools",
                      "UniParc" = "uniparc_id",
                      "Comments" = "comment_count",
                      "Features" = "feature_count"), 
    Interaction = c("Interacts with" = "cc_interaction",
                    "Subunit structure" = "cc_subunit"), 
    Expression = c("Developmental stage" = "cc_developmental_stage",
                   "Induction" = "cc_induction",
                   "Tissue specificity" = "cc_tissue_specificity"), 
    `Gene Ontology (GO)` = c("Gene Ontology (biological process)" = "go_p",
                             "Gene Ontology (cellular component)" = "go_c",
                             "Gene Ontology (GO)" = "go",
                             "Gene Ontology (molecular function)" = "go_f",
                             "Gene Ontology IDs" = "go_id"), 
    `Pathology & Biotech` = c("Allergenic Properties" = "cc_allergen",
                              "Biotechnological use" = "cc_biotechnology",
                              "Disruption phenotype" = "cc_disruption_phenotype",
                              "Involvement in disease" = "cc_disease",
                              "Mutagenesis" = "ft_mutagen",
                              "Pharmaceutical use" = "cc_pharmaceutical",
                              "Toxic dose" = "cc_toxic_dose"), 
    `Subcellular location` = c("Intramembrane" = "ft_intramem",
                               "Subcellular location [CC]" = "cc_subcellular_location",
                               "Topological domain" = "ft_topo_dom",
                               "Transmembrane" = "ft_transmem"), 
    `PTM / Processing` = c("Chain" = "ft_chain",
                           "Cross-link" = "ft_crosslnk",
                           "Disulfide bond" = "ft_disulfid",
                           "Glycosylation" = "ft_carbohyd",
                           "Initiator methionine" = "ft_init_met",
                           "Lipidation" = "ft_lipid",
                           "Modified residue" = "ft_mod_res",
                           "Peptide" = "ft_peptide",
                           "Post-translational modification" = "cc_ptm",
                           "Propeptide" = "ft_propep",
                           "Signal peptide" = "ft_signal",
                           "Transit peptide" = "ft_transit"), 
    Structure = c("3D" = "structure_3d",
                  "Beta strand" = "ft_strand",
                  "Helix" = "ft_helix",
                  "Turn" = "ft_turn"), 
    Publications = c("PubMed ID" = "lit_pubmed_id",
                     "DOI ID" = "lit_doi_id"), 
    `Data of` = c("Date of creation" = "date_created",
                  "Date of last modification" = "date_modified",
                  "Date of last sequence modification" = "date_sequence_modified",
                  "Entry version" = "version"), 
    `Family & Domains` = c("Coiled coil" = "ft_coiled",
                           "Compositional bias" = "ft_compbias",
                           "Domain [CC]" = "cc_domain",
                           "Domain [FT]" = "ft_domain",
                           "Motif" = "ft_motif",
                           "Protein families" = "protein_families",
                           "Region" = "ft_region",
                           "Repeat" = "ft_repeat",
                           "Sequence similarities" = "cc_similarity",
                           "Zinc finger" = "ft_zn_fing"), 
    # External Resources 
    xref_Sequence = c("CCDS" = "xref_ccds",
                      "EMBL" = "xref_embl",
                      "GeneRIF" = "xref_generif",
                      "PIR" = "xref_pir",
                      "RefSeq" = "xref_refseq"), 
    `xref_3D structure` = c("AlphaFoldDB" = "xref_alphafolddb",
                            "BMRB" = "xref_bmrb",
                            "EMDB" = "xref_emdb",
                            "PCDDB" = "xref_pcddb",
                            "PDB" = "xref_pdb_full",
                            "PDBsum" = "xref_pdbsum",
                            "SASBDB" = "xref_sasbdb",
                            "SMR" = "xref_smr"), 
    `xref_Protein-protein interaction` = c("BioGRID" = "xref_biogrid_full",
                                           "CORUM" = "xref_corum",
                                           "ComplexPortal" = "xref_complexportal_full",
                                           "DIP" = "xref_dip",
                                           "ELM" = "xref_elm",
                                           "IntAct" = "xref_intact_full",
                                           "MINT" = "xref_mint",
                                           "STRING" = "xref_string"), 
    xref_Chemisty = NA_character_, 
    `xref_Protein family/group` = NA_character_, 
    xref_PTM = NA_character_, 
    `xref_Genetic variation` = NA_character_, 
    `xref_2D gel` = NA_character_, 
    xref_Proteomic = NA_character_, 
    `xref_Protocols and materials` = NA_character_, 
    `xref_Genome annotation` = c("Ensembl" = "xref_ensembl",
                                 "EnsemblBacteria" = "xref_ensemblbacteria",
                                 "EnsemblFungi" = "xref_ensemblfungi",
                                 "EnsemblMetazoa" = "xref_ensemblmetazoa",
                                 "EnsemblPlants" = "xref_ensemblplants",
                                 "EnsemblProtists" = "xref_ensemblprotists",
                                 "GeneID" = "xref_geneid",
                                 "Gramene" = "xref_gramene",
                                 "KEGG" = "xref_kegg",
                                 "MANE-Select" = "xref_mane-select",
                                 "PATRIC" = "xref_patric",
                                 "UCSC" = "xref_ucsc",
                                 "VectorBase" = "xref_vectorbase",
                                 "WBParaSite" = "xref_wbparasite"), 
    `xref_Organism-specific` = NA_character_, 
    xref_Phylogenomic = NA_character_, 
    `xref_Enzyme and pathway` = NA_character_, 
    xref_Miscellaneous = NA_character_, 
    `xref_Gene expression` = NA_character_, 
    `xref_Family and domain` = c("AntiFam" = "xref_antifam_full",
                                 "CDD" = "xref_cdd_full",
                                 "DisProt" = "xref_disprot",
                                 "FunFam" = "xref_funfam_full",
                                 "Gene3D" = "xref_gene3d_full",
                                 "HAMAP" = "xref_hamap_full",
                                 "IDEAL" = "xref_ideal",
                                 "InterPro" = "xref_interpro_full",
                                 "NCBIfam" = "xref_ncbifam_full",
                                 "PANTHER" = "xref_panther_full",
                                 "PIRSF" = "xref_pirsf_full",
                                 "PRINTS" = "xref_prints_full",
                                 "PROSITE" = "xref_prosite_full",
                                 "Pfam" = "xref_pfam_full",
                                 "SFLD" = "xref_sfld_full",
                                 "SMART" = "xref_smart_full",
                                 "SUPFAM" = "xref_supfam_full"))
  
  return(UniProt_fields)
  
}



#' Identify the type of UniProt data: "sequence_features" (multiple features per
#' protein with sequence position), "protein_feature_multiple" (multiple 
#' features per protein separated by "; "), or "protein_feature_single" (one 
#' term, feature, description per protein)
#'
#' @param vector_UniProt vector of UniProt information like 
#' data_UniProt$info_type
#'
#' @returns
#' @export
#'
#' @examples
identify_UniProt_infotype <- function(vector_UniProt) {
  
  if (any(vector_UniProt %>% 
          na.omit %>% 
          str_detect("/note|/evidence")))
    return("sequence_features")
  else if(any(vector_UniProt %>% 
              na.omit %>% 
              str_detect("; ")))
    return("protein_feature_multiple") 
  else 
    return("protein_feature_single")
}




#' Extract sequence-specific UniProt information from downloaded data 
#'
#' @param data_UniProt data frame of proteins and UniProt info as returned by 
#' get_UniProt_data()
#' @param info_col name of column to be extracted
#' @param remove.na remove proteins without information 
#'
#' @returns
#' @export
#'
#' @examples
extract_UniProt_seqinfo <- function(data_UniProt, 
                                    info_col, 
                                    remove.na = T) {
  
  data_name <- data_UniProt[[info_col]] %>% 
    na.omit() %>% 
    str_extract('^[A-Z]+') %>% 
    unique() 
  
  data_separated <- data_UniProt %>% 
    rename(Info_column = !!info_col) %>% 
    select(1, Info_column) %>% 
    mutate(Info_column = str_replace_all(Info_column, 
                                         paste0("; (?=", data_name, ")"), 
                                         ";here ")) %>% 
    separate_longer_delim(cols = Info_column, 
                          delim = ";here ")
  
  subfields <- data_separated$Info_column %>% 
    str_extract_all('(?<=/)([a-zA-Z_]+)') %>% 
    unlist() %>% 
    na.omit() %>% 
    unique() 
  
  data_output <- data_separated %>% 
    mutate(type = ifelse(!is.na(Info_column), data_name, NA), 
           position = str_extract(Info_column, paste0("(?<=", data_name, " )\\d+\\.\\.\\d+|\\d+")) %>% 
             str_replace("\\.\\.", "-"), 
           from = strsplit_keep_first(position, "-") %>% 
             type.convert(as.is = T), 
           to = strsplit_keep_last(position, "-") %>% 
             type.convert(as.is = T))
  
  for (i in subfields) {
    data_output <- data_output %>% 
      mutate(!!i := str_extract(Info_column, paste0('(?<=/', i, '=")[^"]+')) %||% NA)
  }
  
  if (remove.na) 
    data_output <- data_output %>% 
    filter(!is.na(Info_column))
  
  data_output <- data_output %>% 
    rename(!!info_col := "Info_column")
  
  return(data_output)
  
}


#' Transforms a TERM2GENE data frame into a list of Term sets 
#'
#' @param data_T2G TERM2GENE data frame 
#' @param info_col Term column 
#' @param entry_col Gene column 
#'
#' @returns
#' @export
#'
#' @examples
T2G_2list <- function(data_T2G, 
                      info_col = 1, 
                      entry_col = "Entry") {
  
  if (is.numeric(info_col)) info_col <- names(data_T2G)[info_col]
  
  list_GO <- purrr::map(dplyr::pull(data_T2G, info_col, info_col) %>% 
                          unique() %>% 
                          setNames(., .), 
                        \(x)  data_T2G %>% 
                          dplyr::filter(!!dplyr::sym(info_col) == x) %>% 
                          dplyr::pull(!!entry_col))
  
  return(list_GO)
  
}





