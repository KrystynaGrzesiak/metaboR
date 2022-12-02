
#' Read targeted metabolomic data
#'
#' @description Imports targeted data from a file.
#'
#' @importFrom readxl read_xlsx
#'
#' @inheritParams read_biocrates
#' @param LOD_table A table that contains the values of LOD.
#' \code{\link[metaboR]{read_LOD_table}}.
#'
#' @details This function uses \code{\link[readxl]{read_xlsx}}.
#'
#' @export read_targeted
#'

read_targeted <- function(path, LOD_table, clinical_data = NULL) {

  if(!file.exists(path))
    stop(paste0("The file ", path, " does not exist.
                You probably provided wrong path."))

  dat <- as.data.table(read_xlsx(path))


  dat
}


#' Reads table containing values of limit of detection
#'
#' @description This function imports a table with LOD values.
#'
#' @param path Path to the file. This file should contain a table with a column
#' named `Concentration`  with the values of LOD for every metabolite from the
#' sample.
#'
#' @details This function is a supplementary function for
#' \code{\link[metaboR]{read_targeted}}. It is used to load the LOD table and
#' associate it with the experimental matrix within
#' \code{\link{metaboR_LOD_data}} class.
#'
#' @export read_LOD_table
#'

read_LOD_table <- function(path) {

  if(!file.exists(path))
    stop(paste0("The file ", path, " does not exist.
                You probably provided wrong path."))

  LOD_table <- switch(file_ext(path),
                      "csv" = fread(path, data.table = TRUE),
                      "xlsx" = as.data.table(read_excel(path)),
                      "xls" = as.data.table(read_excel(path)))

  LOD_table

}

