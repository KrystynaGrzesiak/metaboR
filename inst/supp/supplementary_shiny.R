

get_to_remove <- function(to_remove_list) {
  unique(unlist(reactiveValuesToList(to_remove_list)))
}
