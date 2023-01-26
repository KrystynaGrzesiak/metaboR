
library(readxl)
library(data.table)
library(stringr)
library(metaboR)

path <- "./inst/data/targeted/Biocrates/biocrates.xlsx"

LOD_data <- read_biocrates(path)

LOD_data <- remove_sparse_metabolites(LOD_data, LOD_threshold = 0.3)


CV_data <- handle_LOD(LOD_data)

metabolites <- get_CV_to_remove(CV_data)

final_dat <- remove_high_CV(CV_data, metabolites)

class(final_dat)
