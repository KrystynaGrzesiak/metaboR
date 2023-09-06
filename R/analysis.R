#' Performs
#'
#' @description This function
#'
#' @param processed
#'
#' @details This function
#'
#' @export analyze
#'

analyze <- function(processed, group_col, control) {

  clinical_dat <-  attr(processed, "clinical_data")
  subject_id <- attr(clinical_dat, "subject_id")
  setnames(clinical_dat, old = subject_id, new = "Sample_ID")

  full_dat <- merge.data.table(processed, clinical_dat, by = "Sample_ID")

  summary_table <- full_dat[, .(Average = mean(Value)), by = c("Compound", group_col)]

  summary_table[
    , `:=`(p_change = 100 * (Average - unique(Average[get(group_col) == control])) / unique(Average[get(group_col) == control]),
           fold_change = Average/unique(Average[get(group_col) == control])),
    by = Compound
  ]
  summary_table
}
