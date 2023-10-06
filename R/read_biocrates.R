
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

  metabolites <- colnames(dat)[(which(colnames(dat) == "Measurement Time") + 1):ncol(dat)]

  LOD_table <- dat[
    str_extract(`Measurement Time`, "LOD") == "LOD",
    which(colnames(dat) == "Measurement Time"):ncol(dat)
  ]
  setnames(LOD_table, old = c("Measurement Time"), new = c("Plate Bar Code"))
  LOD_table <- LOD_table[, `Plate Bar Code` := str_extract(`Plate Bar Code`, "(\\d)+")]

  setnames(dat, old = c("Sample Identification"), new = c("Sample_ID"))

  cols_to_remove <- colnames(dat)[!(colnames(dat) %in% c("Sample_ID", "Plate Bar Code", "Sample Type", metabolites))]


  dat <- dat[!is.na(`Plate Bar Code`) & (`Sample Type` == "Sample" | grepl("QC", `Sample Type`))]
  dat[, (cols_to_remove) := NULL]

  LOD_table[, `Plate Bar Code` := {
    sets <- unique(dat[["Plate Bar Code"]])
    sapply(`Plate Bar Code`, function(value) {
      sets[grepl(value, sets, fixed = TRUE)][1]
    })
  }]

  LOD_table <- LOD_table[, lapply(.SD, function(col) {
    mean(as.numeric(col))
  }),  by = `Plate Bar Code`]

  LOD_table <- melt(LOD_table, id.vars = "Plate Bar Code",
                    variable.name = "Compound", value.name = "Value")

  setkey(dat, Sample_ID)

  metaboR_LOD_data(dat,
                   LOD_table = LOD_table)
}

