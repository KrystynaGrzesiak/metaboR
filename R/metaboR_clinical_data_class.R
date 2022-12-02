
# VALIDATOR

validate_metaboR_clinical <- function(metaboR_clinical_data) {

  subject_id <- attr(metaboR_clinical_data, "subject_id")

  if(!is.null(subject_id) & !subject_id %in% colnames(metaboR_clinical_data))
    stop(paste0(subject_id, " should be a name of column from clinical data"))
  if(!is.data.frame(metaboR_clinical_data))
    stop("clinical data should be a data frame")

  metaboR_clinical_data

}

# HELPER

metaboR_clinical <- function(clinical_data, subject_id = NULL) {

  clinical_data <- as.data.table(clinical_data)

  if(is.null(subject_id)){
    warning("Subject_id not provcided. It will be taken from the data.")
    subject_id <- colnames(clinical_data)[
      clinical_data[ , lapply(.SD, uniqueN)
      ] == nrow(clinical_data)][1]

    if(is.na(subject_id))
      stop("No unique column in the data. Please, provide subject_id by hand.")
  }

  validate_metaboR_clinical(
    new_metaboR_clinical(clinical_data = clinical_data,
                         subject_id = subject_id)
  )
}


# CREATOR

new_metaboR_clinical <- function(clinical_data, subject_id){
  structure(clinical_data,
            class = c("metaboR_clinical", "data.table", "data.frame"),
            subject_id = subject_id)
}
