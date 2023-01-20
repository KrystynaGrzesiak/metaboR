
### VALIDATOR

validate_metaboR_LOD_data <- function(LOD_data) {

  type <- attr(LOD_data, "type")
  clinical_data <- attr(LOD_data, "clinical_data")
  LOD_table <- attr(LOD_data, "LOD_table")

  # validate analysis type
  if(!(type[1] == "targeted"))
    stop(paste0("Wrong analysis type!
                metaboR_LOD_data class refers targeted analysis. You provided ",
                type[1]))

  type[2] <- match.arg(type[2], c("LC", "GC", "biocrates"))

  #validate metabolomics matrix
  if(uniqueN(LOD_data[`Sample Type` == "Sample"],
             by = key(LOD_data)) != nrow(LOD_data[`Sample Type` == "Sample"]))
    stop("You need to provide unique sample ID.
         There are some duplicated samples in your file.")

  #validate LOD_table GC LC targeted
  # TODO: tutaj sprawdzić czy LOD table jest ok i pasuje do metabolitow

  # validate clinical data
  if(!is.null(clinical_data) & !inherits(clinical_data, "metaboR_clinical"))
    stop("Clinical data should have 'metaboR_clinical' class.")
  if(is.null(clinical_data))
    warning("No clinical data provided.")

  #TODO: sprawdzić czy clinical data zgadza się z macierzą
  # to może być robione w funkcji dla każdych danych klinicznych

  LOD_data

}

### HELPER

metaboR_LOD_data <- function(metabo_matrix,
                             type,
                             LOD_table,
                             clinical_data = NULL) {

  metabo_matrix <- as.data.table(metabo_matrix)
  LOD_table <- as.data.table(LOD_table)

  validate_metaboR_LOD_data(new_metaboR_LOD_data(metabo_matrix = metabo_matrix,
                                                 type = type,
                                                 LOD_table = LOD_table,
                                                 clinical_data = clinical_data))
}


## CREATOR

new_metaboR_LOD_data <- function(metabo_matrix,
                                 type,
                                 LOD_table,
                                 clinical_data = NULL){

  structure(metabo_matrix,
            class = c("metaboR_LOD_data", "data.table", "data.frame"),
            type = type,
            LOD_table = LOD_table,
            clinical_data = clinical_data)
}
