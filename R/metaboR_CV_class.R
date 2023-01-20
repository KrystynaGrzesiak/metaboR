
### VALIDATOR

validate_metaboR_CV_data <- function(CV_data) {
  CV_table <- attr(CV_data, "CV_table")

  if(!setequal(CV_table[, Compound], CV_data[, Compound])) {
    stop("The metabolites provided as metabo_matrix and CV_table are not consistent!")
  }

  CV_data

}

### HELPER

metaboR_CV_data <- function(metabo_matrix,
                            type,
                            CV_table,
                            clinical_data = NULL) {

  metabo_matrix <- as.data.table(metabo_matrix)
  CV_table <- as.data.table(CV_table)

  validate_metaboR_CV_data(new_metaboR_CV_data(metabo_matrix = metabo_matrix,
                                               type = type,
                                               CV_table = CV_table,
                                               clinical_data = clinical_data))
}


## CREATOR

new_metaboR_CV_data <- function(metabo_matrix,
                                type,
                                CV_table,
                                clinical_data = NULL){

  structure(metabo_matrix,
            class = c("metaboR_CV_data", "data.table", "data.frame"),
            type = type,
            CV_table = CV_table,
            clinical_data = clinical_data)
}
