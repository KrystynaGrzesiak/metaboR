
### VALIDATOR

validate_metaboR_LOD_data <- function(LOD_data) {

  matrix <- attr(LOD_data, matrix)
  type <- attr(LOD_data, type)
  clinical_data <- attr(LOD_data, clinical_data)
  LOD_table <- attr(LOD_data, LOD_table)

  # validate analysis type
  if(!(type[1] == "targeted"))
    stop(paste0("Wrong analysis type!
                metaboR_LOD_data class refers targeted analysis. You provided ",
                type[1]))

  type[2] <- match.arg(type[2], c("LC", "GC", "biocrates"))

  #validate LOD_table
  if(nrow(LOD_table) == 0)
    stop("For targeted analysis LOD table needs to be provided.")

  # TODO: tutaj sprawdzić czy LOD table jest ok i pasuje do metabolitow

  # validate clinical data
  if(!is.na(clinical_data) & !inherits(clinical_data, "clinical_data"))
    stop("Clinical data should have 'clinical_data' class.")
  if(is.na(clinical_data))
    warning("No clinical data provided.")

  #TODO: sprawdzić czy clinical data zgadza się z macierzą

  #validate matrix
  if(!is.data.frame(matrix))
    stop("Provided metabolomic matrix should be a data frame.")

  LOD_data

}

### HELPER

metaboR_LOD_data <- function(matrix,
                             type,
                             LOD_table,
                             clinical_data = NA) {

  matrix <- as.data.table(matrix)
  LOD_table <- as.data.table(LOD_table)

  validate_metaboR_LOD_data(new_metaboR_LOD_data(matrix = matrix,
                                                 type = type,
                                                 LOD_table = LOD_table,
                                                 clinical_data = clinical_data))
}


## CREATOR

new_metaboR_LOD_data <- function(matrix,
                                 type,
                                 LOD_table,
                                 clinical_data = NA){

  structure(matrix,
            class = c("metaboR_LOD_data", "data.frame"),
            type = type,
            LOD_table = LOD_table,
            clinical_data = clinical_data)
}
