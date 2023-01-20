
#' Imputing values under the limit of detection LOD in targeted metabolomics
#' analysis
#'
#' @description This function removes sparse metabolites for which the
#' ratio of <LOD values is large, completes <LOD values in accordance to
#' the provided LOD table and calculates CV of QC samples.
#'
#' @param LOD_data an object of metaboR_LOD_data class.
#'
#' @param LOD_threshold a value from 0 to 1 denoting threshold of fraction of
#' LOD in metabolites from the sample. Each metabolite with  <LOD ratio
#' equal or greater than \code{LOD_threshold} will be removed from the data.
#'
#' @details This function returns an object of class `metaboR_CV_data` which
#' contains metabolomics profile in long data format and `CV_table` with
#' coefficient variation calculated for QC samples.
#'
#' @export handle_LOD
#'

handle_LOD <- function(LOD_data, LOD_threshold = 0.3) {

  clinical_data <- attr(LOD_data, "clinical_data")

  LOD_data <- remove_sparse_metabolites(LOD_data, LOD_threshold)
  LOD_data <- complete_LOD(LOD_data)

  CV_table <- LOD_data[`Sample Type` != "Sample", !"Sample_ID"]
  CV_table <- calculate_CV(CV_table)

  LOD_data <- LOD_data[`Sample Type` == "Sample"]

  metaboR_CV_data(LOD_data,
                  CV_table = CV_table,
                  type = c("targeted", "biocrates"),
                  clinical_data = clinical_data)

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

  LOD_data <- melt(LOD_data,
                   id.vars = c("Plate Bar Code", "Sample_ID", "Sample Type"),
                   variable.name = "Compound",
                   value.name = "Value")

  LOD_data <- merge(LOD_data,
                    LOD_table,
                    by = c("Plate Bar Code", "Compound"),
                    suffixes = c("", "_LOD"))

  LOD_data <- LOD_data[, Value := as.numeric(ifelse(Value == "< LOD", Value_LOD, Value))][
    , .(Sample_ID, `Sample Type`,  Compound, Value)
  ]

  LOD_data

}


