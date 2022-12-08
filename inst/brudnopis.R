
library(readxl)
library(data.table)
library(stringr)

path <- "./inst/data/targeted/Biocrates/biocrates.xlsx"

LOD_data <- read_biocrates(path)

LOD_data <- remove_sparse_metabolites(LOD_data, 0.3)

LOD_table <- attr(LOD_data, "LOD_table")



ind <- which(LOD_data == "< LOD", arr.ind = TRUE)

LOD_vals <- unique(merge(data.table(Sample_ID = LOD_data[["Sample_ID"]][ind[, "row"]],
                                    Plate_Code = LOD_data[["Plate Bar Code"]][ind[, "row"]],
                                    Compound = colnames(LOD_data)[ind[, "col"]]),
                         LOD_table,
                         by = c("Plate_Code", "Compound")))





<- LOD_vals[["Value"]]

match(LOD_vals[["Sample_ID"]], LOD_data[["Sample_ID"]])
match(LOD_vals[["Compound"]], colnames(LOD_data))

LOD_vals[["Compound"]]


LOD_data[LOD_vals, on = c(" Sample_ID", "Compound")]



first = c( "a" , "c" , "b", "a", "b" )
second = c( "c" , "b" , "a" )
match(first, second)


