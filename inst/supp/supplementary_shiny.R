

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
