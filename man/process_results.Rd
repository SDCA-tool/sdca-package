% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_results.R
\name{process_results}
\alias{process_results}
\title{Process results}
\usage{
process_results(args, file = FALSE, local = FALSE)
}
\arguments{
\item{args}{a json string as a character}

\item{file}{logical, is json a file path}

\item{local}{logical, are we running locally or on the server}
}
\value{
a json string as a character
}
\description{
Main function that the webtool uses to process resutls from the API
}
\examples{
\dontrun{
process_results("jsonhere")
}
}
