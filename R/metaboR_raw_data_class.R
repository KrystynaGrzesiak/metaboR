

validate_metaboR_raw_data <- function(metaboR_raw_data) {

  raw_data <- attr(metaboR_raw_data, raw_data)
  type <- attr(metaboR_raw_data, type)
  clinical_data <- attr(metaboR_raw_data, clinical_data)

  type <- match.arg(type, c("untargeted_LC", "untargeted_GC", "targeted_LC",
                            "targeted_GC", "Biocrates"))
  if(!is.null(clinical_data) & !inherits(clinical_data, "metaboR_clinical"))
    stop("Clinical data should have 'metaboR_clinical' class.")
  if(!is.data.frame(raw_data))
    stop("Provided raw data should be a data frame")

  metaboR_raw_data
}



metaboR_raw_data <- function(raw_data, type, clinical_data = NULL) {

  raw_data <- as.data.table(raw_data)

  validate_metaboR_raw_data(
    new_metaboR_raw_data(raw_data = raw_data,
                         type = type,
                         clinical_data = clinical_data)
  )
}


new_metaboR_raw_data <- function(raw_data, type, clinical_data){

  structure(raw_data,
            class = c("metaboR_raw_data", "data.frame"),
            type = type,
            clinical_data = clinical_data)
}

