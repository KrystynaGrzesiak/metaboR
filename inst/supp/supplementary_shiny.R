

get_to_remove <- function(to_remove_list) {
  unique(unlist(reactiveValuesToList(to_remove_list)))
}

list_to_dummy_frame <- function(list_dat) {
  max_len <- max(unlist(lapply(list_dat, length)))
  do.call(cbind, lapply(list_dat, function(ith_vec) {
    length(ith_vec) <- max_len
    ith_vec
  }))
}

plot_venn <- function(LOD_ratios, LOD_thresh) {
  groups <- unique(LOD_ratios[["Group"]])
  plt_data <- dcast(LOD_ratios, Compound ~ Group, value.var = "< LOD ratio")
  plt_data <- plt_data[, lapply(.SD, function(ith_col) {
    ifelse(ith_col > LOD_thresh, as.vector(Compound), NA)
  }), .SDcols = groups]

  plt_data <- lapply(as.list(plt_data), na.omit)
  ggvenn(plt_data)
}

