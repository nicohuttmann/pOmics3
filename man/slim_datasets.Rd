% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00e_slim_datasets.R
\name{slim_datasets}
\alias{slim_datasets}
\title{Saves data frames, variables data and observations data as .parquet files 
and replaces entries as loading functions understood by get_ functions}
\usage{
slim_datasets(
  dir = "Data/RData/Datasets",
  replace = T,
  save = T,
  override.variables = T,
  override.observations = T,
  override.data_frames = F
)
}
\arguments{
\item{dir}{folder}

\item{replace}{should data frames be replaced by loading function 
(default = T)}

\item{save}{should data frames be saved as .parquet file to folder
(default = T)}

\item{override.variables}{Should existing variables data be overwritten? 
(default = T)}

\item{override.observations}{Should existing observations data be overwritten? 
(default = T)}

\item{override.data_frames}{Should existing data frames be overwritten? 
(default = F)}
}
\description{
Saves data frames, variables data and observations data as .parquet files 
and replaces entries as loading functions understood by get_ functions
}
