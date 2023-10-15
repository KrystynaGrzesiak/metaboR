
#' Read Biocrates data
#'
#' @description Imports Biocrates data from the `.xlsx` file.
#'
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_extract
#' @importFrom stringr str_to_title
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
#' @keywords internal
#' @noRd
#'

read_biocrates_raw <- function(path) {

  dat <- as.data.table(read_excel(path, skip = 1))
  dat <- metaboR:::fix_names(dat)

  LOD_table_raw <- dat[is.na(`Plate Bar Code`),
                       which(colnames(dat) == "Measurement Time"):ncol(dat)]

  LOD_table <- metaboR:::clean_LOD_table(LOD_table_raw, dat)
  metabolites_names <- metaboR:::get_metabolites_names(dat)
  dat <- metaboR:::remove_redundant_samples(dat)
  biocrates_data <- copy(dat)[, .SD, .SDcols = !c(metabolites_names)]
  dat <- clean_compounds_dat(dat, metabolites_names)

  metaboR_LOD_data(dat,
                   LOD_table = LOD_table,
                   metabolites = metabolites_names,
                   biocrates_data = biocrates_data)
}


############################
############################   SUPPLEMNETARY
############################




#' Prepares LOD data table
#'
#' @keywords internal
#' @noRd
#'

clean_LOD_table <- function(LOD_table, dat) {

  LOD_table <- dat[
    str_extract(`Measurement Time`, "LOD") == "LOD" |
      str_extract(`Measurement Time`, "ULOQ") == "ULOQ" |
      str_extract(`Measurement Time`, "LLOQ") == "LLOQ",
    which(colnames(dat) == "Measurement Time"):ncol(dat)
  ]

  LOD_table[is.na(LOD_table)] <- 0
  setnames(LOD_table, old = c("Measurement Time"), new = c("Plate Bar Code"))

  LOD_table <- LOD_table[, Type := ifelse(grepl("LOD", `Plate Bar Code`),
                                          ifelse(grepl("(calc.)", `Plate Bar Code`),
                                                 "LOD (calc.)", "LOD (from OP)"),
                                          ifelse(grepl("ULOQ", `Plate Bar Code`),
                                                 "ULOQ", "LLOQ"))]
  LOD_table <- LOD_table[, `Plate Bar Code` := str_extract(`Plate Bar Code`, "(\\d)+")]

  LOD_table <- LOD_table[, `Plate Bar Code` := {
    sets <- unique(dat[["Plate Bar Code"]])
    sapply(`Plate Bar Code`, function(value) {
      sets[grepl(value, sets, fixed = TRUE)][1]
    })
  }]

  LOD_table <- melt(LOD_table,
       id.vars = c("Plate Bar Code", "Type"),
       variable.name = "Compound",
       value.name = "Value")

  LOD_table
}



#' Removes samples that are not normal or quality control samples
#'
#' @keywords internal
#' @noRd
#'

remove_redundant_samples <- function(dat) {
  dat[!is.na(`Plate Bar Code`) & (`Sample Type` == "Sample" | grepl("QC", `Sample Type`))]
}

#' This function returns a vector of metabolites names
#'
#' @keywords internal
#' @noRd
#'

get_metabolites_names <- function(dat) {
  colnames(dat)[(which(colnames(dat) == "Measurement Time") + 1):ncol(dat)]
}

#' This function cleans data
#'
#' @keywords internal
#' @noRd
#'

clean_compounds_dat <- function(dat, metabolites_names) {
  setkey(dat, `Sample Identification`)
  dat[dat == "∞"] <- Inf
  dat <- dat[, .SD, .SDcols = c("Plate Bar Code", "Sample Identification", "Sample Type", metabolites_names)]
  dat
}

#' Converts column names to title case
#'
#' @keywords internal
#' @noRd
#'

fix_names <- function(bioc_dat) {
  setnames(bioc_dat, "Measurement time", "Measurement Time", skip_absent = TRUE)

  colnames(bioc_dat)[1:which(colnames(bioc_dat) == "Measurement Time")] <-
    str_to_title(colnames(bioc_dat)[1:which(colnames(bioc_dat) == "Measurement Time")])

  setnames(bioc_dat, "Op", "OP", skip_absent = TRUE)

  bioc_dat
}



