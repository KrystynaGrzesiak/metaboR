
library(readxl)
library(data.table)
library(stringr)


dat <- as.data.table(read_xlsx("inst/data/targeted/Biocrates/biocrates.xlsx", skip = 1))


sets <- dat[str_extract(`Measurement Time`, "LOD") == "LOD", 20:ncol(dat)]
sets[, `Measurement Time` := str_extract(`Measurement Time`, "(\\d)+")]

targeted_dat <- dat[!is.na(`Plate Bar Code`)]


targeted_dat[, .SD, .SDcols = {
  means <- colMeans(dat == "< LOD", na.rm = TRUE) < 20/100
  means | is.na(means)
}]



clinical_dat <- as.data.table(read_xlsx("inst/data/targeted/Biocrates/clinical.xlsx"))



dat <- fread("inst/data/untargeted/LC/neg_Samples80%.txt")
dat[, `Number Passed` := NULL]
dat[dat == 1 & ] <- NA


dat <- as.data.table(read_xlsx("inst/data/untargeted/GC/glejaki_dane.xlsx"))[`Compound Method` != "Name", ]

setnames(dat, c("...2", "...3", "...4"), c("RT", "MZ", "CAS#"))

dat


dat <- as.data.table(read_xlsx("inst/data/targeted/LC/LC-celowane_surowe_dane.xlsx"))





m <- new_metaboR_raw_data(dat, "untargeted_LC", clinical_dat)

attr(m, "clinical_dat")



str(m)



