
### VALIDATOR

validate_metaboR_LOD_data <- function(LOD_data) {

  LOD_table <- attr(LOD_data, "LOD_table")
  LOD_table <- attr(LOD_data, "LOD_table")

  #validate metabolomics matrix
  if(uniqueN(LOD_data[`Sample Type` == "Sample"],
             by = key(LOD_data)) != nrow(LOD_data[`Sample Type` == "Sample"]))
    stop("You need to provide unique sample ID.
         There are some duplicated samples in your file.")

  # TODO: tutaj sprawdzić czy LOD table jest ok i pasuje do metabolitow

  LOD_data

}

### HELPER

metaboR_LOD_data <- function(metabo_matrix,
                             LOD_table,
                             samples_info){

  metabo_matrix <- as.data.table(metabo_matrix)
  LOD_table <- as.data.table(LOD_table)

  validate_metaboR_LOD_data(new_metaboR_LOD_data(metabo_matrix = metabo_matrix,
                                                 LOD_table = LOD_table,
                                                 samples_info = samples_info))
}


## CREATOR

new_metaboR_LOD_data <- function(metabo_matrix,
                                 LOD_table,
                                 samples_info){

  structure(metabo_matrix,
            class = c("metaboR_LOD_data", "data.table", "data.frame"),
            LOD_table = LOD_table,
            samples_info = samples_info)
}
