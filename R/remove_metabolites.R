#' #' Removes metabolites with provided % of < LOD values from the data
#' #'
#' #' @description This function
#' #'
#' #' @param targeted_dat data returned by \code{\link[metaboR]{read_biocrates}},
#' #' \code{\link[metaboR]{read_tLC}} or \code{\link[metaboR]{read_tGC}}.
#' #'
#' #' @param pctg a number denoting <LOD %. The metabolites which contain more
#' #' than \code{pctg} % of  <LOD will be removed from the data.
#' #'
#' #' @details
#' #'
#' #' @keywords internal
#'
#' remove_sparse_metabolites <- function(targeted_dat, pctg) {
#'   targeted_dat[, .SD, .SDcols = {
#'     means <- colMeans(dat == "< LOD", na.rm = TRUE) < pctg/100
#'     means | is.na(means)
#'   }]
#' }
