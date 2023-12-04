

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

  plt_data <- lapply(groups, function(ith_group) {
    ith_group <- as.character(ith_group)
    na.omit(ifelse(plt_data[, ..ith_group ] > LOD_thresh, as.vector(plt_data[["Compound"]]), NA))
  })

  names(plt_data) <- groups
  ggvenn(plt_data)
}

