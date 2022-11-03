

validate_metaboR_clinical <- function(metaboR_clinical) {

  subject_id <- attr(metaboR_clinical, subject_id)

  if(!subject_id %in%colnames(metaboR_clinical))
    stop("subject_id should be a name of column from clinical data")
  if(!is.data.frame(metaboR_clinical))
    stop("clinical data should be a data frame")

  metaboR_clinical

}



metaboR_clinical <- function(clinical_data, subject_id) {

  clinical_data <- as.data.table(clinical_data)

  validate_metaboR_clinical(
    new_metaboR_clinical(clinical_data = clinical_data,
                         subject_id = subject_id)
  )
}




new_metaboR_clinical <- function(clinical_data, subject_id){
  structure(clinical_data,
            class = c("metaboR_clinical", "data.frame"),
            subject_id = subject_id)
}
