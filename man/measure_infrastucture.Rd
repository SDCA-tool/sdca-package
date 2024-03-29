% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measure_infrastucture.R
\name{measure_infrastucture}
\alias{measure_infrastucture}
\title{Measure infrastructure}
\usage{
measure_infrastucture(
  infra,
  assets,
  assets_parameters,
  components,
  carbon_factors,
  material_sites,
  rast_dem,
  rast_bedrock,
  rast_superficial,
  rast_landcover
)
}
\arguments{
\item{infra}{data frame with infrastructure properties}

\item{assets}{a pre-subset data frame}

\item{assets_parameters}{a pre-subset data frame}

\item{components}{a pre-subset data frame}

\item{carbon_factors}{a pre-subset data frame}

\item{material_sites}{a data frame of the nearest material sites}

\item{rast_dem}{path to dem}

\item{rast_bedrock}{path to bedrock}

\item{rast_superficial}{path to superfical}

\item{rast_landcover}{path to landcover}
}
\value{
a json string as a character
}
\description{
Internal function that is to be run on row at a time
}
\examples{
\dontrun{
evaluate_materials()
}
}
