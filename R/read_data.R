#' Read metabolomics data
#'
#' @description Imports metabolomics data corresponding to the analysis type.
#'
#' @importFrom stringr str_extract
#' @import data.table
#'
#' @param path Path to the `xlsx` file.
#' @param type character vector of two elements. First one defines the type of
#' analysis, either `"targeted"` or `"untargeted"`. Second one defined MS type, one
#' of `"LC"`, `"GC"`, `"Biocrates"`.
#' @param orientation specifies orientation of compounds - `"rowwise"` or
#' `"colwise"`
#'
#' @details This function uses ...
#'
#' @keywords internal
#'


read_data <- function(path,
                      type,
                      orientation = NULL,
                      clinical_path = NULL,
                      subject_id = NULL,
                      LOD_table_path = NULL,
                      ...) {

  if(!file.exists(path))
    stop(paste0("The file ", path, " does not exist.
                You probably provided wrong path."))

  type[1] <- match.arg(type[1], c("targeted", "untargeted"))
  type[2] <- match.arg(type[2], c("Biocrates", "LC", "GC"))

  if(type == c("untargeted", "Biocrates"))
    stop("Wrong analysis type. You provided untargeted Biocrates.")

  if(type[2] == "Biocrates"){

    if(!is.null(orientation))
      warning("For Biocrates files provided orientation will be ignored.")

    if(!is.null(LOD_table_path))
      warning("For Biocrates file provided LOD_table will be ignored.")

    LOD_table <- NULL

  } else {

    if(is.null(orientation))
      stop("You must specify the orientation of subjects and metabolites in the data.
         Orientation of compounds should be either 'colwise' or 'rowwise'")
    orientation <- match.arg(orientation, c("rowwise", "colwise"))

    if(type[1] == "targeted"){

      if(is.null(LOD_table_path))
        stop(paste("For the analysis type:", paste(type, collapse = " "),
                   "`LOD_table_path` need to be provided."))

      if(!file.exists(LOD_table_path))
        stop(paste0("The file ", path, " does not exist."))

      if(!file.exists(LOD_table_path))
        stop(paste0("The file ", LOD_table_path, " does not exist."))

      #reading LOD table
      LOD_table <- read_LOD_table(LOD_table_path)
    } else {

      if(!is.null(LOD_table_path))
        warning(paste("For the analysis type:", paste(type, collapse = " "),
                      "`LOD_table_path` will be ignored."))
      LOD_table <- NULL
    }
  }

  if(!is.null(clinical_path)) {

    if(!file.exists(clinical_path))
      stop(paste0("The file ", clinical_path, " does not exist."))

    #reading clinical data
    clinical_data <- read_clinical_data(clinical_path, subject_id)

  } else {
    clinical_data <- NULL
  }

  raw_data <- switch(paste(type, collapse = "_"),
                     #reading metabolomics matrix
                     untargeted_LC = read_untargeted_LC(path, orientation),
                     untargeted_GC = read_untargeted_GC(path, orientation),
                     targeted_LC = read_targeted(path, orientation),
                     targeted_GC = read_targeted(path, orientation),
                     targeted_Biocrates = read_biocrates(path, ...)
  )

  if(type[2] == "Biocrates"){
    metaboR_raw_data(raw_data[["matrix"]],
                     type = type,
                     clinical_data = raw_data[["clinical_data"]],
                     LOD_table = raw_data[["LOD_table"]])
  } else {
    metaboR_raw_data(raw_data,
                     type = type,
                     clinical_data = clinical_data,
                     LOD_table = LOD_table)
  }

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

read_untargeted_LC <- function(path, orientation) {

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

read_untargeted_GC <- function(path) {

  dat <- as.data.table(read_xlsx(path))[`Compound Method` != "Name", ]
  setnames(dat,
           old = c("Compound Method", "...2", "...3", "...4"),
           new = c("Compound", "RT", "MZ", "CAS#"))
  dat <- transpose(dat, keep.names = "Sample_ID", make.names = "Compound")
  dat
}

#' Reads clinical data
#'
#' @importFrom tools file_ext
#'
#' @description This function imports the data containing clinical information
#' about samples.
#'
#' @param path Path to the `xlsx` file. The data contained in \code{path} should
#' have subject id in the first column.  See details for more information.
#' @param subject_id character name of column in the clinical data that contains
#' unique names of subjects. Default NULL. If NULL, then the first unique column
#' from data is taken.
#'
#' @return an onject of metaboR_clinical class
#'
#' @details This function is a supplementary function for
#' \code{\link[metaboR]{read_data}}. It is used to load the clinical data and
#' associate it with the metabolomics matrix  within
#' \code{\link{metaboR_raw_data}} class.
#'
#' @export read_clinical_data
#'

read_clinical_data <- function(path, subject_id = NULL) {

  if(!file.exists(path))
    stop(paste0("The file ", path, " does not exist.
                You probably provided wrong path."))

  clinical_data <- switch(file_ext(path),
                          "csv" = fread(path, data.table = TRUE),
                          "xlsx" = as.data.table(read_excel(path)),
                          "xls" = as.data.table(read_excel(path)))

  metaboR_clinical(clinical_data = clinical_data,
                   subject_id = subject_id)

}





