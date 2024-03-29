
### VALIDATOR

validate_metaboR_processed <- function(processed_data) {


  processed_data

}

### HELPER

metaboR_processed <- function(metabo_matrix,
                              type,
                              clinical_data = NULL) {

  metabo_matrix <- as.data.table(metabo_matrix)

  validate_metaboR_processed(new_metaboR_processed(metabo_matrix = metabo_matrix,
                                                   type = type,
                                                   clinical_data = clinical_data))
}


## CREATOR

new_metaboR_processed <- function(metabo_matrix,
                                  type,
                                  clinical_data = NULL){

  structure(metabo_matrix,
            class = c("metaboR_processed", "data.table", "data.frame"),
            type = type,
            clinical_data = clinical_data)
}
