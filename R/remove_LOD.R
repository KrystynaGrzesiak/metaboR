

# supplementary functions


#' Calculates <LOD ratios per column
#'
#' @description This function creates a table with <LOD ratios ordered by value.
#'
#' @inheritParams handle_LOD
#'

get_LOD_ratios <- function(LOD_data, metabolites = NULL) {

  if(is.null(metabolites))
    metabolites <- attr(LOD_data, "metabolites")

  LOD_ratios <- LOD_data[
    ,.SD, .SDcols = metabolites
  ][, lapply(.SD, function(ith_col) {
    mean(ith_col == "< LOD", na.rm = TRUE)
  })]

  LOD_ratios <- melt(LOD_ratios, measure.vars = colnames(LOD_ratios))

  setnames(LOD_ratios, new = c("Compound", "< LOD ratio"), skip_absent = TRUE)
  setorderv(LOD_ratios, cols = "< LOD ratio", order = -1)

}

#' Gets columns to be removed based on <LOD proportion
#'
#' @inheritParams handle_LOD
#'

get_sparse_columns <- function(LOD_tbl, LOD_threshold)
  as.vector(LOD_tbl[`< LOD ratio` > LOD_threshold, Compound])


#' Removes metabolites with provided ratio of < LOD values from the data
#'
#' @description This function removes metabolites for which the ratio of values
#' under the limit of detection is greater or equal to provided threshold.
#'
#' @inheritParams handle_LOD
#'
#' @param LOD_threshold a value from 0 to 1 denoting threshold of fraction of
#' LOD in metabolites from the sample. Each metabolite with  <LOD ratio
#' equal or greater than \code{LOD_threshold} will be removed from the data.
#'
#' @export remove_sparse_metabolites

remove_sparse_metabolites <- function(LOD_data, LOD_threshold) {
  LOD_tbl <- get_LOD_ratios(LOD_data)
  cols_to_remove <- get_sparse_columns(LOD_tbl, LOD_threshold)

  LOD_data[ , (cols_to_remove) := NULL ]
}



