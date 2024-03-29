% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_materials.R
\name{cacualte_materials}
\alias{cacualte_materials}
\title{Calculate Materials}
\usage{
cacualte_materials(infra, combined, material_sites)
}
\arguments{
\item{infra}{data frame with infrastructure properties}

\item{combined}{a pre-joined data frame}

\item{material_sites}{nearest material site for project}
}
\value{
a list of data frames
}
\description{
Internal function that calculates material emissions
}
\examples{
\dontrun{
evaluate_materials()
}
}
