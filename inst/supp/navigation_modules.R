

get_next_panel <- function(current, app_panels) {
  next_panel <- app_panels[which(app_panels == current) + 1]
  ifelse(is.na(next_panel), current, next_panel)
}

get_prev_panel <- function(current, app_panels) {
  next_panel <- app_panels[which(app_panels == current) - 1]
  ifelse(length(next_panel) == 0, current, next_panel)
}
