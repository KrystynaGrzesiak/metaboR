
### VALIDATOR

validate_metaboR_raw_data <- function(raw_data) {

  matrix <- attr(raw_data, matrix)
  type <- attr(raw_data, type)
  clinical_data <- attr(raw_data, clinical_data)

  # validate analysis type
  type[1] <- match.arg(type[1], c("targeted", "untargeted"))
  type[2] <- match.arg(type[2], c("Biocrates", "LC", "GC"))

  if(type == c("untargeted", "Biocrates"))
    stop("Wrong analysis type. You provided untargeted Biocrates.")

  #validate LOD_table
  if(!nrow(LOD_table) == 0) {
    if(any(type %in% c("Biocrates", "untargeted"))){
      warning(paste("For the provided analysis type:",
                    paste(type, collapse = " "),
                    "LOD_table will be ignored."))
      LOD_table <- as.data.table(NULL)
    }else {
      # targeted LC / GC
      # TODO: tutaj sprawdzić czy LOD table jest ok i pasuje do metabolitow
    }
  }else {
    if(all(type %in% c("targeted", "GC", "LC")))
      stop(paste("For the analysis type:", paste(type, collapse = " "),
                 "LOD_table need to be provided."))
  }

  # validate clinical data
  if(!nrow(clinical_data) == 0 & !inherits(clinical_data, "clinical_data"))
    stop("Clinical data should have 'clinical_data' class.")
  if(is.null(clinical_data))
    warning("No clinical data provided.")

  #TODO: sprawdzić czy clinical data zgadza się z macierzą

  #validate matrix
  if(!is.data.frame(matrix))
    stop("Provided metabolomic matrix should be a data frame.")

  raw_data

}

### HELPER

metaboR_raw_data <- function(raw_data,
                             type,
                             clinical_data = NULL,
                             LOD_table = NULL) {

  matrix <- as.data.table(raw_data)
  clinical_data <- as.data.table(clinical_data)
  LOD_table <- as.data.table(LOD_table)

  validate_metaboR_raw_data(
    new_metaboR_raw_data(matrix = matrix,
                         type = type,
                         clinical_data = clinical_data,
                         LOD_table = LOD_table)
  )
}


## CREATOR

new_metaboR_raw_data <- function(raw_data,
                                 type,
                                 clinical_data = NULL,
                                 LOD_table = NULL){

  structure(matrix,
            class = c("metaboR_raw_data", "data.frame"),
            type = type,
            clinical_data = clinical_data,
            LOD_table = LOD_table)
}
