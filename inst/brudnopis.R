
library(readxl)
library(data.table)
library(stringr)
library(metaboR)

path <- "./inst/data/targeted/Biocrates/biocrates.xlsx"
path1 <- "./inst/data/targeted/Biocrates/clinical.xlsx"

LOD_data <- read_biocrates(path)

LOD_data <- remove_sparse_metabolites(LOD_data, LOD_threshold = 0.3)

CV_data <- handle_LOD(LOD_data)

metabolites <- get_CV_to_remove(CV_data)

final_dat <- remove_high_CV(CV_data, metabolites)

class(final_dat)


# !!!! group col
group_col <- "Rodzaj"
control <- "Kontrola"

analyze(final_dat, group_col, control)


clinical_dat <- attr(final_dat, "clinical_data")
subject_id <- attr(clinical_dat, "subject_id")
setnames(clinical_dat, old = subject_id, new = "Sample_ID")

dat <- merge.data.table(final_dat, clinical_dat, by = "Sample_ID")


dat <- dat[, .(Average = mean(Value)), by = c("Compound", group_col)]

dat[, `:=`(p_change = 100 * (Average - unique(Average[get(group_col) == control])) / unique(Average[get(group_col) == control]),
           fold_change = Average/unique(Average[get(group_col) == control])), by = Compound]







