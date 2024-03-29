#' Calculating coefficient variation (CV) for every metabolite regarding QC
#' levels
#'
#' @description This function calculates CV based on QC data from Biocrates.
#'
#' @param QC_data a table with quality control samples containing columns:
#' `Sample type`, `Compound`  and `Value`
#'
#' @details This function calculates the coefficient variation value based on
#' the formula \eqn{100*\frac{\sqrt{var(qc)}}{\overline{qc}}}.
#'
#' @export calculate_CV
#'

calculate_CV <- function(QC_data) {
  dcast(QC_data, Compound ~ `Sample Type`, value.var = "Value",
        fun.aggregate = function(value) 100 * sd(value) / mean(value))
}


#' Remove metabolites based on the list
#'
#' @description This function removes metabolites from the data based on
#' provided list.
#'
#' @inheritParams get_CV_to_remove
#' @param metabolites a character vector of names of metabolites to remove from
#' the data
#'
#' @details This function returns an object of one of three classes:
#'
#' - metaboR_imputation when the type of analysis is GC or LC untargeted
#'
#' - metaboR_derivatives when the type of analysis is GC targeted
#'
#' - metaboR_processed when the type of analysis is Lc targeted or biocrates
#'
#' @export remove_high_CV
#'

remove_high_CV <- function(CV_data, metabolites) {
  type <- attr(CV_data, "type")
  clinical_data <- attr(CV_data, "clinical_data")

  metabo_matrix <- CV_data[!(Compound %in% metabolites)]

  # LC and GC untargeted
  if(type[1] == "untargeted")
    return(metaboR_imputation())

  # GC targeted
  if(type[2] == "GC")
    return(metaboR_derivatives())

  # biocrates and LC targeted left
  metaboR_processed(metabo_matrix,
                    type = type,
                    clinical_data = clinical_data)

}


#' Get metabolites with high CV
#'
#' @description This function returns metabolites which should be removed based
#' on CV value
#'
#' @param CV_data an object of metaboR_CV_data class.
#' @param CV_threshold a percentage threshold value of coefficient variation.
#' This function will return the names of metabolites CV value larger than
#' \code{CV_threshold}.
#'
#' @export get_CV_to_remove
#'

get_CV_to_remove <- function(CV_data, CV_threshold = 30) {
  CV_table <- attr(CV_data, "CV_table")
  as.vector(CV_table[`QC Level 2` >= CV_threshold, Compound])
}

