% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data.R
\name{read_data}
\alias{read_data}
\title{Read metabolomics data}
\usage{
read_data(
  path,
  type,
  orientation = NULL,
  clinical_path = NULL,
  subject_id = NULL,
  LOD_table_path = NULL,
  ...
)
}
\arguments{
\item{path}{Path to the `xlsx` file.}

\item{type}{character vector of two elements. First one defines the type of
analysis, either `"targeted"` or `"untargeted"`. Second one defined MS type, one
of `"LC"`, `"GC"`, `"Biocrates"`.}

\item{orientation}{specifies orientation of compounds - `"rowwise"` or
`"colwise"`}
}
\description{
Imports metabolomics data corresponding to the analysis type.
}
\details{
This function uses ...
}
\keyword{internal}
