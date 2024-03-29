% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_biocrates.R
\name{read_biocrates}
\alias{read_biocrates}
\title{Read Biocrates data}
\usage{
read_biocrates(path, keep_cols = "none", clinical_data = NULL)
}
\arguments{
\item{path}{Path to the `.xlsx` file.}

\item{keep_cols}{character vector. Indicates whether the
variables "Sample Bar Code", "Sample Description",  "Submission Name",
"Collection Date", "Species", "Material", "OP",  "Org. Info",
"Plate Production No.", "gender", "sample type", "Plate Note",
"Well Position", "Sample Volume", "Run Number", "Injection Number",
"Measurement Time" should be left as a part of clinical data. You can
provide either `all` indicating that all of the mentioned variables should
remain in the data, `none` indicating that all should be removed or a
character vector of names of columns that should remain. Note that the
variables "Sample Type" and "Sample Identification" will not be removed.}

\item{clinical_data}{clinical data of metaboR_clinical class. Use
\code{\link[metaboR]{metaboR_clinical}} to read clinical data.}
}
\description{
Imports Biocrates data from the `.xlsx` file.
}
\details{
This function uses \code{\link[readxl]{read_xlsx}}.
Use \code{\link[metaboR]{metaboR_clinical}} to read clinical data.
}
