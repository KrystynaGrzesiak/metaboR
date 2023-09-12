
#' Read Biocrates data
#'
#' @description Imports Biocrates data from the `.xlsx` file.
#'
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_extract
#'
#' @param path Path to the `.xlsx` file.
#' @param keep_cols character vector. Indicates whether the
#' variables "Sample Bar Code", "Sample Description",  "Submission Name",
#' "Collection Date", "Species", "Material", "OP",  "Org. Info",
#' "Plate Production No.", "gender", "sample type", "Plate Note",
#' "Well Position", "Sample Volume", "Run Number", "Injection Number",
#' "Measurement Time" should be left as a part of clinical data. You can
#' provide either `all` indicating that all of the mentioned variables should
#' remain in the data, `none` indicating that all should be removed or a
#' character vector of names of columns that should remain. Note that the
#' variables "Sample Type" and "Sample Identification" will not be removed.

#' @param clinical_data clinical data of metaboR_clinical class. Use
#' \code{\link[metaboR]{metaboR_clinical}} to read clinical data.
#'
#' @details This function uses \code{\link[readxl]{read_xlsx}}.
#' Use \code{\link[metaboR]{metaboR_clinical}} to read clinical data.
#'
#' @export read_biocrates
#'


read_biocrates <- function(path, keep_cols = "none") {

  if(!file.exists(path))
    stop(paste0("The file ", path, " does not exist.
                You probably provided wrong path."))

  read_biocrates_raw(path, keep_cols)
}


#' Read Biocrates data
#'
#' @keywords internal app
#' @noRd
#'

read_biocrates_raw <- function(path, keep_cols = "none") {

  dat <- as.data.table(read_excel(path, skip = 1))

  LOD_table <- dat[str_extract(`Measurement Time`, "LOD") == "LOD", 20:ncol(dat)]
  setnames(LOD_table, old = c("Measurement Time"), new = c("Plate Bar Code"))
  LOD_table <- LOD_table[, `Plate Bar Code` := str_extract(`Plate Bar Code`, "(\\d)+")]

  setnames(dat, old = c("Sample Identification"), new = c("Sample_ID"))

  dat <- dat[!is.na(`Plate Bar Code`)]

  samples_info <- list(
    QC_levels = dat[`Sample Type` != "Sample", `Sample Type`],
    Submission_Names = dat[, `Submission Name`],
    species = unique(dat[, `Species`]),
    OP = unique(dat[, `OP`]),
    Material = unique(dat[, `Material`]),
    gender = dat[, `gender`],
    sample_type = dat[, `sample type`],
    Sample_Volume = unique(dat[, `Sample Volume`])
  )

  cols_info <- clean_biocrates(keep_cols)
  cols_to_remove <- cols_info[["cols_to_remove"]]
  cols_to_save <- c("Sample_ID", cols_info[["cols_to_save"]])

  LOD_table[, `Plate Bar Code` := {
    sets <- unique(dat[["Plate Bar Code"]])
    sapply(`Plate Bar Code`, function(value) {
      sets[grepl(value, sets, fixed = TRUE)]
    })
  }]

  LOD_table <- LOD_table[, lapply(.SD, function(col) {
    mean(as.numeric(col))
  }),  by = `Plate Bar Code`]

  LOD_table <- melt(LOD_table, id.vars = "Plate Bar Code",
                    variable.name = "Compound", value.name = "Value")

  clinical_to_add <- dat[, ..cols_to_save]

  dat <- dat[ , .SD, .SDcols = !cols_to_remove]
  setkey(dat, Sample_ID)

  metaboR_LOD_data(dat,
                   LOD_table = LOD_table,
                   samples_info = samples_info)
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
  additional_cols <- c("Sample Bar Code", "Sample Description",
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
