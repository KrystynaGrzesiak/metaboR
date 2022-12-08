
#' Imputing values under the limit of detection LOD in targeted metabolomics
#' analysis
#'
#' @description This function removes sparse metabolites for which the
#' ratio of <LOD values is large and completes <LOD values in accordance to
#' the provided LOD table.
#'
#' @param LOD_data an object of metaboR_LOD_data class.
#'
#' @param LOD_threshold a value from 0 to 1 denoting threshold of fraction of
#' LOD in metabolites from the sample. Each metabolite with  <LOD ratio
#' equal or greater than \code{LOD_threshold} will be removed from the data.
#'
#' @details
#'
#' @export handle_LOD
#'

handle_LOD <- function(LOD_data, LOD_threshold = 0.3) {

  LOD_data <- remove_sparse_metabolites(LOD_data, LOD_threshold)


}


# supplementary functions

#' Removes metabolites with provided ratio of < LOD values from the data
#'
#' @description This function removes metabolites for which the ratio of values
#' under the limit of detection is greater or equal to provided threshold.
#'
#' @inheritParams handle_LOD
#'
#' @export remove_sparse_metabolites

remove_sparse_metabolites <- function(LOD_data, LOD_threshold) {
  LOD_data[, .SD, .SDcols = {
    means <- colMeans(LOD_data == "< LOD", na.rm = TRUE) < LOD_threshold
    means | is.na(means)
  }]
}

#' complete values under the limit of detection in targeted analysis
#'
#' @description This function uses the table of limit of detection for every
#' metabolite in the data and completes all the <LOD values with 1/2LOD from
#' provided table.
#'
#' @inheritParams handle_LOD
#'
#' @keywords internal

complete_LOD <- function(LOD_data) {
  LOD_table <- attr(LOD_data, "LOD_table")

  metabolites <- colnames(LOD_data)[-c(1:2)]

  LOD_data[ , metabolites := lapply(.SD, function(var) {
    browser()

    limit_value <- LOD_table

    LOD_data

  }), .SDcols = metabolites]




  apply(LOD_data, 1, function(row_data) {
    LOD_row <- unlist(LOD_table[Plate_Code == row_data[1], -c("Plate_Code")])
    row_data[row_data == "< LOD"] <- LOD_row[names(row_data[row_data == "< LOD"])]
    row_data
  })


  LOD_data[ , function(.SD) {
              LOD_row <- LOD_table[Concentration == .SD[1], -c("Concentration")]
              .SD[as.vector(.SD == "<LOD")] <- LOD_row[as.vector(.SD[-1] == "<LOD")]
            }, by = .I]


  targeted_dat[, .SD, .SDcols = {
    means <- colMeans(dat == "< LOD", na.rm = TRUE) < pctg/100
    means | is.na(means)
  }]
}


