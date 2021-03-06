% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkAlevinInputFiles.R
\name{checkAlevinInputFiles}
\alias{checkAlevinInputFiles}
\title{Check that all required input files are available}
\usage{
checkAlevinInputFiles(baseDir)
}
\arguments{
\item{baseDir}{Path to the output directory from the alevin run (should be
the directory containing the \code{alevin} directory).}
}
\value{
Returns nothing, raises an error if any of the required files are
  missing.
}
\description{
Check that all required input files are available
}
\examples{
checkAlevinInputFiles(system.file("extdata/alevin_example_v0.14",
                                  package = "alevinQC"))

}
\author{
Charlotte Soneson
}
