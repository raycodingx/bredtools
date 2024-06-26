% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sortped.R
\name{sortped}
\alias{sortped}
\title{Sort a pedigree}
\usage{
sortped(ped, addgen = TRUE)
}
\arguments{
\item{ped}{A data.table or data frame including the pedigree, in which the names of the first three columns are \strong{Ind}, \strong{Sire} and \strong{Dam}. Missing parent has been replaced with the default missing value \strong{NA}.}

\item{addgen}{A logical value indicates whether the individual generation number will be inferred. The default values is TRUE, then a new column named \strong{Gen} will be added in the returned data.table.}
}
\value{
A data.table including the resorted pedigree is returned. The individual generation is inferred and a new column \strong{Gen} is added when the parameter \emph{addgen} is TRUE. The Gen column is integer.
}
\description{
\code{sortped} function assigns generations and sorts a pedigree .
}
\details{
This function takes a pedigree, assigns individuals to different generations, sort parents before offspring.
}
