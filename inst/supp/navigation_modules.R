




observe_next <- function(tab_name, input, session, panels) {
  btn_name <- paste0("right_", tab_name)
  observeEvent(input[[btn_name]], {
    current <- input[["general_tab"]]

    updateTabsetPanel(session = session,
                      inputId = "general_tab",
                      selected = panels[which(panels == current) + 1])
  })
}


next_panel_btns <- function(input, session, panels) {
  observe_next("Start", input, session, panels)
  observe_next("Players", input, session, panels)
  observe_next("Playlist", input, session, panels)
  observe_next("Game", input, session, panels)
}


observe_prev <- function(tab_name, input, session, panels) {
  btn_name <- paste0("left_", tab_name)
  observeEvent(input[[btn_name]], {
    current <- input[["general_tab"]]

    updateTabsetPanel(session = session,
                      inputId = "general_tab",
                      selected = panels[which(panels == current) - 1])
  })
}


prev_panel_btns <- function(input, session, panels) {
  observe_prev("Players", input, session, panels)
  observe_prev("Playlist", input, session, panels)
  observe_prev("Game", input, session, panels)
}
