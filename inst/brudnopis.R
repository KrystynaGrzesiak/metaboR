
library(readxl)
library(data.table)
library(stringr)

path <- "./inst/data/targeted/Biocrates/biocrates.xlsx"

LOD_data <- read_biocrates(path)

LOD_data <- remove_sparse_metabolites(LOD_data, 0.3)

clinical_data <- attr(LOD_data, "clinical_data")
LOD_table <- attr(LOD_data, "LOD_table")

#
#
# ind <- which(LOD_data == "< LOD", arr.ind = TRUE)
#
# LOD_vals <- unique(merge(data.table(Sample_ID = LOD_data[["Sample_ID"]][ind[, "row"]],
#                                     Plate_Code = LOD_data[["Plate Bar Code"]][ind[, "row"]],
#                                     Compound = colnames(LOD_data)[ind[, "col"]]),
#                          LOD_table,
#                          by = c("Plate_Code", "Compound")))
#
#
#
# data.frame(Sample_ID = match(LOD_vals[["Sample_ID"]], LOD_data[["Sample_ID"]]),
#            metabolite = match(LOD_vals[["Compound"]], colnames(LOD_data)))

LOD_data <- melt(LOD_data, id.vars = c("Plate Bar Code", "Sample_ID", "Sample Type"),
                 variable.name = "Compound", value.name = "Value")


LOD_data <- merge(LOD_data,
                  LOD_table,
                  by = c("Plate Bar Code", "Compound"),
                  suffixes = c("", "_LOD"))

LOD_data <- LOD_data[, Value := ifelse(Value == "< LOD", Value_LOD, Value)][, .(Sample_ID, `Sample Type`,  Compound, Value)]

QC_data <- LOD_data[`Sample Type` != "Sample"]
LOD_data <- LOD_data[`Sample Type` == "Sample"]



a <- dcast(LOD_data, Sample_ID ~ Compound, value.var = "Value")

