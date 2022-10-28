
read_data <- function(path, type, orientation = NULL) {

  if(!file.exists("path"))
    stop("This file does not exist.")

  type <- match.arg(type, c("untargeted_LC", "untargeted_GC", "targeted_LC",
                            "targeted_GC", "biocrates"))
  if(type == "biocrates" & !is.null(orientation))
    warning("For Biocrates files provided orientation will be ignored.")

  if(type != "biocrates" & is.null(orientation))
    stop("You must specify the orientation of subjects and metabolites in your data.")

  dat <- switch(type,
                untargeted_LC = read_uLC(path),
                untargeted_GC = read_uGC(path),
                targeted_LC = read_tLC(path),
                targeted_GC = read_tGC(path),
                biocrates = read_biocrates(path)
  )

  dat

}



read_uLC <- function() {

}

read_uGC <- function() {

}

read_tLC <- function() {

}

read_tGC <- function() {

}


#' Read Biocrates data
#'
#' @description Imports Biocrates data from the `.xlsx` file.
#'
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_extract
#'
#' @param path Path to the `xlsx` file.
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
}



#' Removes metabolites with large % of < LOD values from the data
#'
#' @param targeted_dat data returned by read_biocrates, read_tLC or read_tGC
#' @param pctg a number denoting <LOD %.  The metabolites which contain more
#' than \code{pctg} % of  <LOD will be removed from the data.
#'
#'
#' @keywords internal
#'

remove_sparse_metabolites <- function(targeted_dat, pctg) {
  targeted_dat[, .SD, .SDcols = {
    means <- colMeans(dat == "< LOD", na.rm = TRUE) < pctg/100
    means | is.na(means)
  }]
}




