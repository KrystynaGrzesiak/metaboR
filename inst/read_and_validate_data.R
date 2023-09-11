
read_data_app <- function(file_input) {

  ext <- tools::file_ext(file_input[["datapath"]])
  req(file_input)

  validate(need(ext %in% c("xlsx", "xls"), "Please upload a xlsx or xls file"))

  biocrates_data <- read_biocrates_raw(path = file_input[["datapath"]])

  biocrates_data
}







