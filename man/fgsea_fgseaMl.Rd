% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03c_fgsea.R
\name{fgsea_fgseaMl}
\alias{fgsea_fgseaMl}
\title{Title}
\usage{
fgsea_fgseaMl(
  stats,
  pathways,
  sampleSize = 101,
  minSize = 1,
  maxSize = length(stats) - 1,
  eps = 1e-50,
  scoreType = c("std", "pos", "neg"),
  nproc = 0,
  gseaParam = 1,
  BPPARAM = NULL,
  nPermSimple = 1000,
  absEps = NULL,
  silent = T,
  seed = 42
)
}
\arguments{
\item{stats}{Named vector of gene-level stats. Names should be the same as in 'pathways'}

\item{pathways}{List of gene sets to check.}

\item{sampleSize}{The size of a random set of genes which in turn has size = pathwaySize}

\item{minSize}{Minimal size of a gene set to test. All pathways below the threshold are excluded.}

\item{maxSize}{Maximal size of a gene set to test. All pathways above the threshold are excluded.}

\item{eps}{This parameter sets the boundary for calculating the p value.}

\item{scoreType}{This parameter defines the GSEA score type.
Possible options are ("std", "pos", "neg").
By default ("std") the enrichment score is computed as in the original GSEA.
The "pos" and "neg" score types are intended to be used for one-tailed tests
(i.e. when one is interested only in positive ("pos") or negateive ("neg") enrichment).}

\item{nproc}{If not equal to zero sets BPPARAM to use nproc workers (default = 0).}

\item{gseaParam}{GSEA parameter value, all gene-level statis are raised to the power of `gseaParam`
before calculation of GSEA enrichment scores.}

\item{BPPARAM}{Parallelization parameter used in bplapply.
Can be used to specify cluster to run. If not initialized explicitly or
by setting `nproc` default value `bpparam()` is used.}

\item{nPermSimple}{Number of permutations in the simple fgsea implementation
for preliminary estimation of P-values.}

\item{absEps}{deprecated, use `eps` parameter instead}

\item{silent}{(default = T) Should warning messages be supressed?}

\item{seed}{Seed to use for reproducible results}
}
\description{
Title
}
