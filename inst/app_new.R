library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)

library(ggplot2)
library(ggiraph)
library(patchwork)

library(metaboR)

library(readxl)
library(openxlsx)

library(data.table)
library(DT)

source("supp/ui_supp.R")
source("supp/custom_dt.R")
source("supp/navigation_modules.R")
source("supp/supplementary_shiny.R")



app_panels <- c("upload_data", "remove_metabolites", "quality_control",
                "summary", "download")


ui <- navbarPage(
  title = "Metabocrates",
  theme = shinytheme("sandstone"),

  tabPanel("About", ui_content_about()),
  tabPanel(
    "Run",
    display_navigation_bar(
      steps = c("Upload data",
                "Remove metabolites with high LOD proportion",
                "Quality control",
                "Summary statistics",
                "Download")
    ),

    tabsetPanel(
      id = "app_panel",
      type = "hidden",
      ###############Load data
      tabPanel(
        "upload_data",
        column(3,
               style = "background-color:#f8f5f0; border-right: 1px solid",
               br(),
               h4("Upload new data"),
               fileInput(
                 inputId = 'new_data_path',
                 label = "Upload Biocrates® file.",
                 multiple = FALSE,
                 accept = c(".xlsx", ".xls")
               ),
               br(),
               h4("... or load your previous project"),
               fileInput(
                 inputId = 'project_path',
                 label = "Upload Excel sheet downloaded from MetaboCrates.",
                 multiple = FALSE,
                 accept = c(".xlsx", ".xls")
               )
        ),
        column(9,
               tabsetPanel(
                 tabPanel(
                   "Data summary",
                   br(),
                   htmlOutput("samples_info"),
                   br(),
                   withSpinner(
                     girafeOutput("samples_info_plt"),
                     color = "#3e3f3a")
                 ),
                 tabPanel(
                   "Dataset preview",
                   br(),
                   h4("You can see metabolomics matrix and LOD table below:"),
                   br(),
                   tabsetPanel(
                     tabPanel(
                       "Metabolites",
                       withSpinner(DT::dataTableOutput("biocrates_data"),
                                   color = "#3e3f3a")
                     ),
                     tabPanel(
                       "LOD values",
                       withSpinner(DT::dataTableOutput("LOD_table"),
                                   color = "#3e3f3a")
                     )
                   )
                 )
               )
        )
      ),
      ############### Remove metabolites
      tabPanel(
        "remove_metabolites",
        tabsetPanel(
          tabPanel(
            "Remove based on <LOD",
            column(4,
                   style = "background-color:#f8f5f0; border-right: 1px solid",
                   br(),
                   h4("Here you can remove metabolites with high level of <LOD values."),
                   br(),
                   sliderInput("LOD_thresh",
                               "Select maximum ratio of <LOD allowed for each metabolite and click Remove button!",
                               value = 0.2,
                               min = 0.01,
                               max = 1,
                               step = 0.01,
                               ticks = FALSE),
                   br(),
                   fluidRow(
                     column(1,
                            align = "center",
                            offset = 1,
                            actionButton("remove_btn", label = "Remove")),
                     column(1,
                            offset = 1,
                            align = "center",
                            actionButton("undo_btn",label = "Restore"))
                   ),
                   br(),
                   h4("The following metabolites will be removed:"),
                   br(),
                   htmlOutput("to_remove_names"),
                   br()
            ),
            column(4,
                   offset = 2,
                   h4("Ratio of <LOD values per metabolite:"),
                   shinycssloaders::withSpinner(
                     DT::dataTableOutput("LOD_ratios_tbl"),
                     color = "#3e3f3a"
                   )
            )
          ),
          tabPanel(
            "Manual removal",
            column(4,
                   style = "background-color:#f8f5f0; border-right: 1px solid",
                   br(),
                   h4("Select metabolites from the table on the right and click Remove!"),
                   br(),
                   fluidRow(
                     column(1,
                            align = "center",
                            offset = 1,
                            actionButton("remove_hand_btn", label = "Remove")),
                     column(1,
                            offset = 1,
                            align = "center",
                            actionButton("undo_hand_btn",label = "Restore"))
                   ),
                   br(),
                   h4("Selected:"),
                   htmlOutput("to_remove_by_hand_names"),
                   br()
            ),
            column(4, offset = 2,
                   h4("Ratio of <LOD values per metabolite:"),
                   shinycssloaders::withSpinner(
                     DT::dataTableOutput("LOD_ratios_hand_tbl"),
                     color = "#3e3f3a"
                   )
            )
          )
        ),
      ),
      ############### Quality control
      tabPanel(
        "quality_control",
        tabsetPanel(
          tabPanel(
            "Remove based on CV",
            column(4,
                   style = "background-color:#f8f5f0; border-right: 1px solid",
                   br(),
                   h4("Here you can remove metabolites with high level of CV values."),
                   br(),
                   sliderInput("CV_thresh",
                               "Provide threshold (%) for CV values (QC Level):",
                               value = 30,
                               min = 1,
                               max = 100,
                               step = 1,
                               ticks = FALSE),
                   br(),
                   selectInput("QC_type",
                               label = "Select referential QC level:",
                               choices = c(),
                               selected = c()),
                   br(),
                   fluidRow(
                     column(1,
                            align = "center",
                            offset = 1,
                            actionButton("remove_CV_btn", label = "Remove")),
                     column(1,
                            offset = 1,
                            align = "center",
                            actionButton("undo_CV_btn",label = "Restore"))
                   ),
                   br(),
                   h4("The following metabolites will be removed."),
                   br(),
                   htmlOutput("to_remove_CV_names"),
                   br()
            ),
            column(5, offset = 1,
                   h4("Table of coefficient variation:"),
                   shinycssloaders::withSpinner(
                     DT::dataTableOutput("CV_tbl"),
                     color = "#3e3f3a"
                   )
            )
          ),
          tabPanel(
            "Manual removal",
            column(4,
                   style = "background-color:#f8f5f0; border-right: 1px solid",
                   br(),
                   h4("Select metabolites from the table on the right and click Remove!"),
                   br(),
                   fluidRow(
                     column(1,
                            align = "center",
                            offset = 1,
                            actionButton("remove_CV_hand_btn", label = "Remove")),
                     column(1,
                            offset = 1,
                            align = "center",
                            actionButton("undo_CV_hand_btn",label = "Restore"))
                   ),
                   br(),
                   h4("Selected:"),
                   htmlOutput("to_remove_CV_hand_names"),
                   br()
            ),
            column(5, offset = 1,
                   h4("Table of coefficient variation:"),
                   shinycssloaders::withSpinner(
                     DT::dataTableOutput("CV_ratios_hand_tbl"),
                     color = "#3e3f3a"
                   )
            )
          )
        )
      ),
      tabPanel("summary"),
      tabPanel("download")
    ),

  ),
  tabPanel("Download")
)

server <- function(input, output, session) {

  dat <- reactiveValues()
  to_remove <- reactiveValues()
  tmp_vars <- reactiveValues()

  ################ loading data
  observeEvent(input[["new_data_path"]], {
    req(input[["new_data_path"]])

    file <- input[["new_data_path"]]
    path <- file[["datapath"]]
    ext <- tools::file_ext(path)

    validate(
      need(ext %in% c("xlsx", "xls"),
           paste("Please upload an .xlsx or .xls file! You provided", ext))
    )

    try({ raw_data <- metaboR:::read_biocrates_raw(path) })

    dat[["raw_data"]] <- raw_data
    dat[["n_cmp"]] <- uniqueN(attr(raw_data, "LOD_table")[, Compound])
    dat[["n_smp"]] <- nrow(raw_data)
    dat[["removed_LOD"]] <- copy(raw_data)
    dat[["LOD_ratios"]] <- metaboR:::get_LOD_ratios(dat[["removed_LOD"]])
    dat[["completed_data"]] <- metaboR:::handle_LOD(dat[["removed_LOD"]])
    dat[["removed_CV"]] <- dat[["completed_data"]]
  })

  ####### info
  output[["samples_info"]] <- renderUI({
    req(dat[["raw_data"]])
    req(dat[["n_smp"]])
    req(dat[["n_cmp"]])
    get_raw_html_content(info = attr(dat[["raw_data"]], "samples_info"),
                         n_smp = dat[["n_smp"]],
                         n_cmp = dat[["n_cmp"]])
  })

  output[["samples_info_plt"]] <- renderGirafe({
    req(dat[["raw_data"]])

    info <- attr(dat[["raw_data"]], "samples_info")
    girafe(code = print(plot_raw_info(dat[["raw_data"]])),
           options = list(opts_sizing(rescale = FALSE)),
           width_svg = 16,
           height_svg = 5)
  })


  ###### Data preview
  output[["biocrates_data"]] <- DT::renderDataTable({
    req(dat[["raw_data"]])
    custom_datatable(dat[["raw_data"]],
                     scrollY = 550,
                     paging = FALSE)
  })

  output[["LOD_table"]] <- DT::renderDataTable({
    req(dat[["raw_data"]])
    custom_datatable(attr(dat[["raw_data"]], "LOD_table"),
                     scrollY = 550,
                     paging = FALSE)
  })

  ################ navigation

  observeEvent(input[["next"]], {
    updateTabsetPanel(session,
                      inputId = "app_panel",
                      selected = get_next_panel(input[["app_panel"]],
                                                app_panels))
  })

  observeEvent(input[["prev"]], {
    updateTabsetPanel(session,
                      inputId = "app_panel",
                      selected = get_prev_panel(input[["app_panel"]],
                                                app_panels))
  })


  ################ removing LOD

  output[["LOD_ratios_tbl"]] <-  DT::renderDataTable({
    custom_datatable(dat[["LOD_ratios"]], scrollY = 550, paging = FALSE)
  })

  to_remove_LOD <- reactive({
    req(dat[["LOD_ratios"]])
    req(input[["LOD_thresh"]])
    metaboR:::get_sparse_columns(dat[["LOD_ratios"]], input[["LOD_thresh"]])
  })

  output[["to_remove_names"]] <- renderUI({
    get_remove_html_content(to_remove_LOD())
  })

  observeEvent(input[["remove_btn"]], {
    req(dat[["removed_LOD"]])
    to_remove[["LOD"]] <- to_remove_LOD()

    if(length(to_remove_LOD()) > 0) {
      dat[["removed_LOD"]] <- dat[["removed_LOD"]][ , (get_to_remove(to_remove)) := NULL ]
      dat[["LOD_ratios"]] <- metaboR:::get_LOD_ratios(dat[["removed_LOD"]])
    }
  })

  observeEvent(input[["undo_btn"]], {
    req(dat[["raw_data"]])
    req(dat[["removed_LOD"]])

    to_remove[["LOD"]] <- NULL
    dat[["removed_LOD"]] <- copy(dat[["raw_data"]])

    total_removing <- get_to_remove(to_remove)

    if(!is.null(total_removing))
      dat[["removed_LOD"]] <- dat[["removed_LOD"]][, (total_removing) := NULL ]

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
      to_remove[["LOD_hand"]] <- c(to_remove[["LOD_hand"]], to_remove_by_hand())
      dat[["removed_LOD"]] <- dat[["removed_LOD"]][, (to_remove_by_hand()) := NULL ]
      dat[["LOD_ratios"]] <- metaboR:::get_LOD_ratios(dat[["removed_LOD"]])
    }
  })

  observeEvent(input[["undo_hand_btn"]], {
    req(dat[["raw_data"]])
    req(dat[["removed_LOD"]])

    to_remove[["LOD_hand"]] <- NULL
    dat[["removed_LOD"]] <- copy(dat[["raw_data"]])

    total_removing <- get_to_remove(to_remove)

    if(!is.null(total_removing))
      dat[["removed_LOD"]] <- dat[["removed_LOD"]][ , (total_removing) := NULL ]

    dat[["LOD_ratios"]] <- metaboR:::get_LOD_ratios(dat[["removed_LOD"]])
  })

  output[["to_remove_by_hand_names"]] <- renderUI({
    get_remove_html_content(unlist(to_remove_by_hand()))
  })

  ################ Quality control


  output[["CV_tbl"]] <-  DT::renderDataTable({
    req(dat[["removed_CV"]])

    dat[["removed_CV"]] <- metaboR:::remove_high_CV(dat[["removed_CV"]], get_to_remove(to_remove))
    dat[["CV_ratios"]] <- attr(dat[["removed_CV"]], "CV_table")

    QC_levels <- colnames(dat[["CV_ratios"]])[-1]
    updateSelectInput(session,
                      inputId = "QC_type",
                      choices = QC_levels,
                      selected = QC_levels[2])


    custom_datatable(setorderv(dat[["CV_ratios"]], input[["QC_type"]], order = -1),
                     scrollY = 550,
                     paging = FALSE)
  })

  to_remove_CV <- reactive({
    req(input[["CV_thresh"]])
    req(input[["QC_type"]])

    metaboR:::get_CV_to_remove(dat[["removed_CV"]],
                               input[["CV_thresh"]],
                               QC_type = input[["QC_type"]])
  })


  output[["to_remove_CV_names"]] <- renderUI({
    get_remove_html_content(to_remove_CV())
  })


  observeEvent(input[["remove_CV_btn"]], {
    req(dat[["removed_CV"]])
    to_remove[["CV"]] <- to_remove_CV()

    if(length(to_remove_CV()) > 0) {
      dat[["removed_CV"]] <- metaboR:::remove_high_CV(dat[["removed_CV"]], to_remove_CV())
      dat[["CV_ratios"]] <- attr(dat[["removed_CV"]], "CV_table")
    }
  })


  observeEvent(input[["undo_CV_btn"]], {
    req(dat[["completed_data"]])
    req(dat[["removed_CV"]])

    to_remove[["CV"]] <- NULL
    dat[["removed_CV"]] <- copy(dat[["completed_data"]])

    total_removing <- get_to_remove(to_remove)

    if(!is.null(total_removing)){
      dat[["removed_CV"]] <- metaboR:::remove_high_CV(dat[["removed_CV"]], total_removing)
      dat[["CV_ratios"]] <- attr(dat[["removed_CV"]], "CV_table")
    }
    dat[["CV_ratios"]] <- attr(dat[["removed_CV"]], "CV_table")
  })



  to_remove_CV_by_hand <- reactive({
    to_remove_ind <- input[["CV_ratios_hand_tbl_rows_selected"]]
    as.vector(dat[["CV_ratios"]][to_remove_ind, Compound])
  })

  output[["CV_ratios_hand_tbl"]] <-  DT::renderDataTable({
    DT::datatable(setorderv(dat[["CV_ratios"]], input[["QC_type"]], order = -1),
                  editable = FALSE,
                  selection = list(mode = "multiple", target = "row"),
                  options = list(paging = FALSE,
                                 scrollX = TRUE,
                                 scrollY = 550,
                                 pageLength = 15,
                                 searching = FALSE))
  })

  observeEvent(input[["remove_CV_hand_btn"]], {
    req(dat[["CV_ratios"]])

    to_remove[["CV_hand"]] <- c(to_remove[["CV_hand"]], to_remove_CV_by_hand())

    if(length(to_remove_CV_by_hand()) > 0) {
      dat[["removed_CV"]] <- metaboR:::remove_high_CV(dat[["removed_CV"]], to_remove[["CV_hand"]])
      dat[["CV_ratios"]] <- attr(dat[["removed_CV"]], "CV_table")
    }
  })

  observeEvent(input[["undo_CV_hand_btn"]], {
    req(dat[["removed_CV"]])

    to_remove[["CV_hand"]] <- NULL
    dat[["removed_CV"]] <- metaboR:::remove_high_CV(copy(dat[["completed_data"]]), get_to_remove(to_remove))
    dat[["CV_ratios"]] <- attr(dat[["removed_CV"]], "CV_table")

    total_removing <- get_to_remove(to_remove)

    if(!is.null(total_removing)){
      dat[["removed_CV"]] <- metaboR:::remove_high_CV(dat[["removed_CV"]],total_removing)
      dat[["CV_ratios"]] <- attr(dat[["removed_CV"]], "CV_table")
    }

    dat[["CV_ratios"]] <- attr(dat[["removed_CV"]], "CV_table")
  })

  output[["to_remove_CV_hand_names"]] <- renderUI({
    get_remove_html_content(unlist(to_remove_CV_by_hand()))
  })

}

shinyApp(ui, server)








