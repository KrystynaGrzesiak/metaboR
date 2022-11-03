#' Read metabolomics data
#'
#' @description Imports metabolomics data corresponding to the analysis type.
#'
#' @importFrom stringr str_extract
#' @import data.table
#'
#' @param path Path to the `xlsx` file.
#' @param type character name of MS type. One of `"untargeted_LC"`,
#' `"untargeted_GC"`, `"targeted_LC"`, `"targeted_GC"` and `"biocrates"`.
#' @param orientation specifies orientation of compounds - "rowwise" or "colwise"
#'
#' @details This function uses ...
#'
#' @keywords internal
#'



read_data <- function(path, type, clinical_path = NULL, orientation = NULL) {

  if(!file.exists("path"))
    stop("This file does not exist.")

  type <- match.arg(type, c("untargeted_LC", "untargeted_GC", "targeted_LC",
                            "targeted_GC", "biocrates"))
  # if(type == "biocrates" & !is.null(orientation))
  #   warning("For Biocrates files provided orientation will be ignored.")
  #
  # if(type != "biocrates" & is.null(orientation))
  #   stop("You must specify the orientation of subjects and metabolites in your data.")

  if(!file.exists(path))
    stop(paste0("The file ", path, " does not exist."))

  if(!is.null(clinical_path)) {
    if(!file.exists(clinical_path))
      stop(paste0("The file ", clinical_path, " does not exist."))
    else
      clinical_data <- read_clinical_data(clinical_path)
  } else{
    clinical_data <- NULL
  }

  dat <- switch(type,
                untargeted_LC = read_uLC(path),
                untargeted_GC = read_uGC(path),
                targeted_LC = read_tLC(path),
                targeted_GC = read_tGC(path),
                biocrates = read_biocrates(path)
  )

  metaboR_raw_data(dat,
                   type = type,
                   clinical_data = clinical_data)

}

#' Read untargeted LC data
#'
#' @description Imports untargeted LC data from the `.txt` file.
#'
#' @inheritParams read_data
#'
#' @details This function uses \code{\link[data.table]{fread}}
#'
#' @keywords internal
#'

read_uLC <- function(path) {
  dat <- fread(path)
  dat[, `Number Passed` := NULL]
  dat[dat == 1] <- NA

  dat
}

#' Read untargeted GC data
#'
#' @description Imports untargeted GC data from the `.xlsx` file.
#'
#' @inheritParams read_data
#'
#' @details This function uses \code{\link[readxl]{read_xlsx}}
#'
#' @keywords internal
#'

read_uGC <- function(path) {
  dat <- as.data.table(read_xlsx(path))[`Compound Method` != "Name", ]
  setnames(dat, c("...2", "...3", "...4"), c("RT", "MZ", "CAS#"))
  dat
}

#' Read targeted LC data
#'
#' @description Imports untargeted LC data from the `.xlsx` file.
#'
#' @inheritParams read_data
#'
#' @details This function uses \code{\link[readxl]{read_xlsx}}
#'
#' @keywords internal
#'

read_tLC <- function(path) {
  dat <- as.data.table(read_xlsx(path))
  dat
}

#' Read targeted GC data
#'
#' @description Imports untargeted LC data from the `.xlsx` file.
#'
#' @inheritParams read_data
#'
#' @details This function uses \code{\link[readxl]{read_xlsx}}
#'
#' @keywords internal
#'

read_tGC <- function(path) {

}


#' Read Biocrates data
#'
#' @description Imports Biocrates data from the `.xlsx` file.
#'
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_extract
#'
#' @inheritParams read_data
#'
#' @details This function uses \code{\link[readxl]{read_xlsx}}
#'
#' @keywords internal
#'


read_biocrates <- function(path) {
  dat <- as.data.table(read_xlsx(path, skip = 1))

  sets <- dat[str_extract(`Measurement Time`, "LOD") == "LOD", 20:ncol(dat)]
  sets[, `Measurement Time` := str_extract(`Measurement Time`, "(\\d)+")]

  dat <- dat[!is.na(`Plate Bar Code`)]

  #tutaj zwrócić zarówno dat jak i sets

}




#' Reads clinical data
#'
#' @description This function imports the data containing clinical information
#' about samples.
#'
#' @param path Path to the `xlsx` file. Default NULL, denoting no clinical data
#' regarding considered metabolomics matrix. See details for more information.
#'
#' @details The data contained in \code{path} should have
#' subject id in the first column.
#'
#' This function is a supplementary function for
#' \code{\link[metaboR]{read_data}}. It is used to load the clinical data and
#' associate it with the metabolomics matrix  within
#' \code{\link{metaboR_raw_data}} class.
#'
#' @export read_clinical_data
#'

read_clinical_data <- function(path) {

  clinical_data <- read_xlsx(path)
  subject_id <- colnames(clinical_data)[1]

  metaboR_clinical(clinical_data = clinical_data,
                   subject_id = subject_id)

}






