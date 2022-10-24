
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
#' @description Imports Biocrates data from the `.xlsx` file
#'
#' @importFrom readxl read_xlsx
#'
#' @param path aaaa
#'
#' @details This function uses \code{\link[readxl]{read_xlsx}}
#'
#' @keywords internal
#'


read_biocrates <- function(path) {


}






