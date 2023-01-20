
library(readxl)
library(data.table)
library(stringr)
library(metaboR)

path <- "./inst/data/targeted/Biocrates/biocrates.xlsx"

LOD_data <- read_biocrates(path)

CV_data <- handle_LOD(LOD_data, LOD_threshold = 0.3)

metabolites <- get_CV_to_remove(CV_data)

remove_high_CV(CV_data, metabolites)
