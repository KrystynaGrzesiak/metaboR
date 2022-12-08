
#' Read Biocrates data
#'
#' @description Imports Biocrates data from the `.xlsx` file.
#'
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_extract
#'
#' @param path Path to the `.xlsx` file.
#' @param keep_cols character vector. Indicates whether the
#' variables "Sample Bar Code", "Sample Type", "Sample Description",
#' "Submission Name", "Collection Date", "Species", "Material", "OP",
#' "Org. Info", "Plate Production No.", "gender", "sample type", "Plate Note",
#' "Well Position", "Sample Volume", "Run Number", "Injection Number",
#' "Measurement Time" should be left as a part of clinical data. You can
#' provide either `all` indicating that all of the mentioned variables should
#' remain in the data, `none` indicating that all should be removed or a
#' character vector of names of columns that should remain.
#' @param clinical_data clinical data of metaboR_clinical class. Use
#' \code{\link[metaboR]{metaboR_clinical}} to read clinical data.
#'
#' @details This function uses \code{\link[readxl]{read_xlsx}}.
#' Use \code{\link[metaboR]{metaboR_clinical}} to read clinical data.
#'
#' @export read_biocrates
#'


read_biocrates <- function(path, keep_cols = "none", clinical_data = NULL) {

  if(!file.exists(path))
    stop(paste0("The file ", path, " does not exist.
                You probably provided wrong path."))

  dat <- as.data.table(read_xlsx(path, skip = 1))

  LOD_table <- dat[str_extract(`Measurement Time`, "LOD") == "LOD", 20:ncol(dat)]
  setnames(LOD_table, old = c("Measurement Time"), new = c("Plate_Code"))
  LOD_table <- LOD_table[, Plate_Code := str_extract(Plate_Code, "(\\d)+")]

  setnames(dat, old = c("Sample Identification"), new = c("Sample_ID"))

  cols_info <- clean_biocrates(keep_cols)
  cols_to_remove <- cols_info[["cols_to_remove"]]
  cols_to_save <- c("Sample_ID", cols_info[["cols_to_save"]])

  dat <- dat[!is.na(`Plate Bar Code`)]

  LOD_table[, Plate_Code := {
    sets <- unique(dat[["Plate Bar Code"]])
    sapply(Plate_Code, function(value) {
      sets[grepl(value, sets, fixed = TRUE)]
    })
  }]
  LOD_table <- LOD_table[, lapply(.SD, function(col) {
    mean(as.numeric(col))
  }),  by = Plate_Code]

  LOD_table <- melt(LOD_table, id.vars = "Plate_Code",
       variable.name = "Compound", value.name = "Value")

  clinical_to_add <- dat[, ..cols_to_save]

  if(!is.null(clinical_data)){
    clinical_data <- validate_metaboR_clinical(clinical_data)
    subject_id <- attr(clinical_data, "subject_id")
    clinical_to_add <- merge(clinical_data,
                             clinical_to_add,
                             by.x = subject_id,
                             by.y = "Sample_ID")
  }

  clinical_data <- metaboR_clinical(clinical_to_add,
                                    subject_id = "Sample_ID")

  dat <- dat[ , .SD, .SDcols = !cols_to_remove]

  metaboR_LOD_data(dat,
                   type = c("targeted", "biocrates"),
                   LOD_table = LOD_table,
                   clinical_data = clinical_data)
}


#' Supplementary cleaning for biocrates data
#'
#' @inheritParams read_biocrates
#'
#' @details This function returns a character vector of names of columns to be
#' removed from the biocrates data.
#' @keywords internal
#'

clean_biocrates <- function(keep_cols) {
  additional_cols <- c("Sample Bar Code", "Sample Type", "Sample Description",
                       "Submission Name", "Collection Date", "Species",
                       "Material", "OP", "Org. Info", "Plate Production No.",
                       "gender", "sample type", "Plate Note", "Well Position",
                       "Sample Volume", "Run Number", "Injection Number",
                       "Measurement Time")
  if(keep_cols == "all")
    cols_to_save <- additional_cols
  if(keep_cols == "none")
    cols_to_save <- c()

  if(any(keep_cols %in% additional_cols)) {
    cols_to_save <- match.arg(keep_cols, additional_cols)

    if(length(cols_to_save) != length(keep_cols))
      warning(paste("You provided wrong names. The variables",
                    paste(setdiff(keep_cols, cols_to_save), collapse = ", "),
                    "cannot be found in your data."))
  }
  cols_to_remove <- setdiff(additional_cols,
                            cols_to_save)

  list(cols_to_remove = cols_to_remove,
       cols_to_save = cols_to_save)

}
