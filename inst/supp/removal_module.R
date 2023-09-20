
module_SERVER <- function(id) {
  moduleServer(id, function(input, output, session, dat) {

    output[["LOD_ratios_tbl"]] <-  DT::renderDataTable({
      custom_datatable(dat[["LOD_ratios"]], scrollY = 550, paging = FALSE)
    })

    to_remove_LOD <- reactive({
      req(dat[["LOD_ratios"]])
      req(input[["LOD_thresh"]])
      metaboR:::get_sparse_columns(dat[["LOD_ratios"]], input[["LOD_thresh"]])
    })

    output[["sparse_to_remove"]] <- renderUI({
      get_remove_html_content(to_remove_LOD())
    })

    observeEvent(input[["remove_btn"]], {
      req(dat[["removed_LOD"]])
      dat[["to_remove_LOD"]] <- to_remove_LOD()

      if(length(to_remove_LOD()) > 0) {
        dat[["removed_LOD"]] <- dat[["removed_LOD"]][ , (to_remove_LOD()) := NULL ]
        dat[["LOD_ratios"]] <- metaboR:::get_LOD_ratios(dat[["removed_LOD"]])
      }
    })

    observeEvent(input[["undo_btn"]], {
      req(dat[["raw_data"]])
      req(dat[["removed_LOD"]])
      dat[["to_remove_LOD"]] <- NULL
      dat[["removed_LOD"]] <- copy(dat[["raw_data"]])

      if(!is.null(dat[["to_remove_LOD_hand"]]))
        dat[["removed_LOD"]] <- dat[["removed_LOD"]][, (dat[["to_remove_LOD_hand"]]) := NULL ]

      dat[["LOD_ratios"]] <- metaboR:::get_LOD_ratios(dat[["removed_LOD"]])
    })

    to_remove_by_hand <- reactive({
      req(dat[["LOD_ratios"]])
      to_remove_ind <- input[["LOD_ratios_hand_tbl_rows_selected"]]
      as.vector(dat[["LOD_ratios"]][to_remove_ind, Compound])
    })

    output[["LOD_ratios_hand_tbl"]] <-  DT::renderDataTable({
      DT::datatable(dat[["LOD_ratios"]],
                    editable = FALSE,
                    selection = list(mode = "multiple", target = "row"),
                    options = list(paging = FALSE,
                                   scrollX = TRUE,
                                   scrollY = 550,
                                   pageLength = 15,
                                   searching = FALSE))
    })

    observeEvent(input[["remove_hand_btn"]], {
      req(dat[["LOD_ratios"]])
      req(dat[["removed_LOD"]])

      if(length(to_remove_by_hand()) > 0) {
        dat[["to_remove_LOD_hand"]] <- c(dat[["to_remove_LOD_hand"]], to_remove_by_hand())
        dat[["removed_LOD"]] <- dat[["removed_LOD"]][, (to_remove_by_hand()) := NULL ]
        dat[["LOD_ratios"]] <- metaboR:::get_LOD_ratios(dat[["removed_LOD"]])
      }
    })

    observeEvent(input[["undo_hand_btn"]], {
      req(dat[["raw_data"]])
      req(dat[["removed_LOD"]])
      dat[["to_remove_LOD_hand"]] <- NULL
      dat[["removed_LOD"]] <- copy(dat[["raw_data"]])

      if(!is.null(dat[["to_remove_LOD"]]))
        dat[["removed_LOD"]] <- dat[["removed_LOD"]][ , (dat[["to_remove_LOD"]]) := NULL ]

      dat[["LOD_ratios"]] <- metaboR:::get_LOD_ratios(dat[["removed_LOD"]])
    })

    output[["sparse_to_remove_by_hand"]] <- renderUI({
      get_remove_html_content(unlist(to_remove_by_hand()))
    })

  })


}
