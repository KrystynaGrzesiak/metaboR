

custom_datatable <- function(dat, paging = TRUE, scrollY = 380) {
  DT::datatable(dat,
                editable = FALSE,
                selection = list(selectable = FALSE),
                options = list(paging = paging,
                               scrollX = TRUE,
                               scrollY = scrollY,
                               pageLength = 15,
                               searching = FALSE),
                class = "display nowrap",)
}
