% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/99_data_handling_helpers.R
\name{tt}
\alias{tt}
\title{Transposes tibble and uses first column as column names}
\usage{
tt(data, to.row.names)
}
\arguments{
\item{data}{tibble}

\item{to.row.names}{row names column after transposing (if first column is 
"variables" or "observations", it will automatically assume the other)}
}
\description{
Transposes tibble and uses first column as column names
}
