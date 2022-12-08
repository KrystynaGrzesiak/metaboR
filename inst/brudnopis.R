
library(readxl)
library(data.table)
library(stringr)

path <- "./inst/data/targeted/Biocrates/biocrates.xlsx"

LOD_data <- read_biocrates(path)

LOD_data <- remove_sparse_metabolites(LOD_data, 0.3)

LOD_table <- attr(LOD_data, "LOD_table")
