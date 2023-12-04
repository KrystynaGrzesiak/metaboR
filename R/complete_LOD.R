


#' Imputing values under the limit of detection LOD in targeted metabolomics
#' analysis
#'
#' @description This function removes sparse metabolites for which the
#' ratio of <LOD values is large, completes <LOD values in accordance to
#' the provided LOD table and calculates CV of QC samples.
#'
#' @param LOD_data an object of metaboR_LOD_data class.
#'
#' @details This function returns an object of class `metaboR_CV_data` which
#' contains metabolomics profile in long data format and `CV_table` with
#' coefficient variation calculated for QC samples.
#'
#' @export handle_LOD
#'

handle_LOD <- function(LOD_data,
                       LOD_table = NULL,
                       LOD_frac = 0.5,
                       LOD_type = "calc") {

  if(is.null(LOD_table))
    LOD_table <- attr(LOD_data, "LOD_table")

  LOD_type <- match.arg(LOD_type, c("calc", "op"))
  LOD_type <- ifelse(LOD_type == "calc", "LOD (calc.)", "LOD (from OP)")

  LOD_data <- complete_LOD(LOD_data, LOD_table, LOD_frac, LOD_type)

  CV_table <- LOD_data[`Sample Type` != "Sample", !"Sample Identification"]
  CV_table <- calculate_CV(CV_table)

  LOD_data <- LOD_data[`Sample Type` == "Sample"]

  metaboR_CV_data(LOD_data,
                  CV_table = CV_table)

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

complete_LOD <- function(LOD_data, LOD_table, LOD_frac, LOD_type) {

  if(!(LOD_type %in% unique(LOD_table[["Type"]])))
    LOD_type <- unique(LOD_table[["Type"]])[1]

  LOD_data <- melt(LOD_data,
                   id.vars = c("Plate Bar Code",
                               "Sample Identification",
                               "Sample Type"),
                   variable.name = "Compound",
                   value.name = "Value")

  LOD_table[, "Value"] <- as.numeric(unlist(LOD_table[, "Value"]))

  LOD_to_impute <- LOD_table[Type == LOD_type,
                             .(LOD_Value = sum(Value, na.rm = TRUE) * LOD_frac),
                             by = list(`Plate Bar Code`, Compound)]

  LOD_to_impute <- LOD_to_impute[
    , LLOQ_Value := LOD_table[Type == "LLOQ",
                              .(LLOQ = sum(Value, na.rm = TRUE) * LOD_frac,
                                ULOQ = sum(Value, na.rm = TRUE)),
                              by = list(`Plate Bar Code`, Compound)][, LLOQ ]
  ]
  LOD_to_impute <- LOD_to_impute[
    , ULOQ_Value := LOD_table[Type == "ULOQ",
                              .(ULOQ = sum(Value, na.rm = TRUE)),
                              by = list(`Plate Bar Code`, Compound)][, ULOQ]
  ]

  LOD_to_impute <- LOD_to_impute[
    , which(unlist(lapply(LOD_to_impute, function(x)!all(is.na(x))))), with = F
  ]

  LOD_data <- merge(LOD_data,
                    LOD_to_impute,
                    by = c("Plate Bar Code", "Compound"))

  LOD_data <- LOD_data[
    , Value := as.numeric(
      ifelse(Value == "< LOD", LOD_Value,
             ifelse(Value == "< LLOQ", LLOQ_Value,
                    ifelse(Value == "> ULOQ", ULOQ_Value, Value)))
    )
  ][
    , .(`Sample Identification`, `Sample Type`,  Compound, Value)
  ]

  LOD_data

}
