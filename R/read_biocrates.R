
#' Read Biocrates data
#'
#' @description Imports Biocrates data from the `.xlsx` file.
#'
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_extract
#'
#' @param path Path to the `.xlsx` file.
#'
#' @details This function uses \code{\link[readxl]{read_xlsx}}.
#' Use \code{\link[metaboR]{metaboR_clinical}} to read clinical data.
#'
#' @export read_biocrates
#'


read_biocrates <- function(path) {

  if(!file.exists(path))
    stop(paste0("The file ", path, " does not exist.
                You probably provided wrong path."))


  read_biocrates_raw(path)
}


#' Read Biocrates data
#'
#' @keywords internal app
#' @noRd
#'

read_biocrates_raw <- function(path) {

  dat <- as.data.table(read_excel(path, skip = 1))

  LOD_table <- dat[
    str_extract(`Measurement Time`, "LOD") == "LOD",
    which(colnames(dat) == "Measurement Time"):ncol(dat)
  ]
  setnames(LOD_table, old = c("Measurement Time"), new = c("Plate Bar Code"))
  LOD_table <- LOD_table[, `Plate Bar Code` := str_extract(`Plate Bar Code`, "(\\d)+")]

  setnames(dat, old = c("Sample Identification"), new = c("Sample_ID"))

  dat <- dat[!is.na(`Plate Bar Code`) & (`Sample Type` == "Sample" | grepl("QC", `Sample Type`))]

  LOD_table[, `Plate Bar Code` := {
    sets <- unique(dat[["Plate Bar Code"]])
    sapply(`Plate Bar Code`, function(value) {
      sets[grepl(value, sets, fixed = TRUE)][1]
    })
  }]
#
#   LOD_table <- LOD_table[, lapply(.SD, function(col) {
#     mean(as.numeric(col))
#   }),  by = `Plate Bar Code`]

  LOD_table <- melt(LOD_table, id.vars = "Plate Bar Code",
                    variable.name = "Compound", value.name = "Value")

  setkey(dat, Sample_ID)

  metabolites_names <- colnames(dat)[(which(colnames(dat) == "Measurement Time") + 1):ncol(dat)]
  dat[dat == "∞"] <- Inf

  metaboR_LOD_data(dat,
                   LOD_table = LOD_table,
                   metabolites = metabolites_names)
}

