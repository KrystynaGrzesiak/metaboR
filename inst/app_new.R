library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)

library(ggplot2)
library(ggiraph)
library(patchwork)
library(ggfortify)

library(metaboR)

library(readxl)
library(openxlsx)

library(data.table)
library(DT)

source("supp/ui_supp.R")
source("supp/custom_dt.R")
source("supp/navigation_modules.R")
source("supp/supplementary_shiny.R")



app_panels <- c("upload_data", "data_setup", "remove_metabolites", "quality_control", "download")


ui <- navbarPage(
  id = "main_navbar",
  title = "Metabocrates",
  theme = shinytheme("sandstone"),

  tabPanel("About", ui_content_about()),
  tabPanel(
    "Run",
    display_navigation_bar(
      steps = c("Upload data",
                "Data setup",
                "Remove metabolites with high LOD proportion",
                "Quality control",
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
               h4("... or load your previous project"),
               fileInput(
                 inputId = 'project_path',
                 label = "Upload Excel sheet downloaded from MetaboCrates.",
                 multiple = FALSE,
                 accept = c(".xlsx", ".xls")
               )
        ),
        column(9,
               h3("Dataset preview"),
               h4("You can see metabolomics matrix and LOD table below:"),
               br(),
               tabsetPanel(
                 tabPanel("Data summary",
                          br(),
                          htmlOutput("raw_data_summary"),
                 ),
                 tabPanel(
                   "Compounds",
                   withSpinner(DT::dataTableOutput("biocrates_matrix"),
                               color = "#3e3f3a")
                 ),
                 tabPanel(
                   "Experiment setup data",
                   withSpinner(DT::dataTableOutput("biocrates_full_data"),
                               color = "#3e3f3a")
                 ),
                 tabPanel(
                   "LOD values",
                   withSpinner(DT::dataTableOutput("LOD_table"),
                               color = "#3e3f3a")
                 )
               )
        ),
      ),
      ##################### Setup
      tabPanel(
        "data_setup",
        column(
          3,
          style = "background-color:#f8f5f0; border-right: 1px solid",
          br(),
          h4("1. Select <LOD treatment strategy:"),
          radioButtons(
            inputId = "LOD_strategy",
            label = "Replace <LOD values with",
            choiceValues = c("zero","1/2", "1/sqrt2", "detection limit"),
            choiceNames = c("zero (not recommended)","1/2 detection limit", "1/√2 detection limit", "detection limit"),
            selected = "zero"
          ),
          br(),
          h4("2. Provide variable containing experimental group:"),
          h5(HTML("<b>Select the column with experimental group from the table (optional). Click again to unselect. </b>")),
          htmlOutput("selected_group"),
          br(),
          h4("3. Select imputation methods:"),
          selectInput("missing_imputation_method",
                      "Select missing values imputation method (optional)", choices = ""),
          selectInput("infinite_imputation_method",
                      "Select infinite values (∞) imputation method (optional)", choices = "")
        ),
        column(7, offset = 1,
               h3("Select column containing grouping variable from the table below."),
               h4("You cannot choose variables with NA's."),
               br(),
               withSpinner(DT::dataTableOutput("group_columns"),
                           color = "#3e3f3a")
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
          ),
          tabPanel(
            "Principal Component Analysis",
            h3("Principal Component Analysis"),
            br(),
            column(10, offset = 1,
                   shinycssloaders::withSpinner(
                     plotOutput("PCA_QC", height = "550px"),
                     color = "#3e3f3a"
                   )
            )
          )
        )
      ),
      tabPanel("download",
               h3("Great! Go to Download panel to download your data!"))
    ),

  ),
  tabPanel("Download",
           h2("Here you can download your results at any step of your work!"),
           br(),
           h3("Click the button below to download results."),
           downloadButton("download_excel", "Download", style = "width:20%;"),
           br(),
           br(),
           fluidRow(column(4,
                           h4("Removed metabolites:"),
                           br(),
                           htmlOutput("to_remove_total"),
           ))
  )
)


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


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

    raw_data <- NULL

    validate(
      need(ext %in% c("xlsx", "xls"),
           paste("Please upload an .xlsx or .xls file! You provided", ext))
    )

    raw_data <- tryCatch(
      expr = metaboR:::read_biocrates_raw(path),
      error = function(e) e
    )

    if("error" %in% class(raw_data)) {
      if(raw_data[["message"]] == ("You need to provide unique sample ID.
         There are some duplicated samples in your file."))
        sendSweetAlert(session, title = "Oops!",
                       text = raw_data[["message"]],
                       type = "error")
      else
        sendSweetAlert(session, title = "Oops!",
                       text = "Something went wrong! Is your data valid?",
                       type = "error")

      req(dat[["raw_data"]])
    } else {
      # sendSweetAlert(session, title = "Success!",
      #                text = "Your data is correct!",
      #                type = "success")
    }

    dat[["raw_data"]] <- raw_data
    dat[["LOD_table"]] <- attr(raw_data, "LOD_table")
    dat[["metabolites"]] <- attr(raw_data, "metabolites")
    dat[["biocrates_data"]] <- attr(raw_data, "biocrates_data")

    dat[["n_cmp"]] <- length(dat[["metabolites"]])
    dat[["n_smp"]] <- nrow(raw_data)

    dat[["removed_LOD"]] <- copy(raw_data)
    dat[["LOD_ratios"]] <- metaboR:::get_LOD_ratios(dat[["removed_LOD"]])

    updateRadioButtons(session, "LOD_strategy", selected = "1/2")
  })


  observeEvent(input[["project_path"]], {
    req(input[["project_path"]])

    file <- input[["project_path"]]
    path <- file[["datapath"]]
    ext <- tools::file_ext(path)

    validate(
      need(ext %in% c("xlsx", "xls"),
           paste("Please upload an .xlsx or .xls file! You provided", ext))
    )
    try({
      sheet_names <- setdiff(excel_sheets(path = path),
                             c("Data", "Removed", "n_cmp", "n_smp"))
      for(sheet in sheet_names) {
        dat[[sheet]] <- as.data.table(read_xlsx(path, sheet = sheet))
      }
      project_data <- read_xlsx(path, sheet = "Data")
      removed <- read_xlsx(path, sheet = "Removed")
      dat[["n_cmp"]] <- unlist(read_xlsx(path, sheet = "n_cmp", col_names = FALSE))
      dat[["n_smp"]] <- unlist(read_xlsx(path, sheet = "n_smp", col_names = FALSE))
    })

    to_remove[["LOD"]] <-  as.vector(na.omit(removed[["LOD"]]))
    to_remove[["LOD_hand"]] <-  as.vector(na.omit(removed[["LOD_hand"]]))
    to_remove[["CV"]] <-  as.vector(na.omit(removed[["CV"]]))
    to_remove[["CV_hand"]] <-  as.vector(na.omit(removed[["CV_hand"]]))
  })


  ###### Data preview

  output[["raw_data_summary"]] <- renderUI({
    req(dat[["raw_data"]])
    req(dat[["biocrates_data"]])
    req(dat[["n_cmp"]])
    req(dat[["n_smp"]])

    info_dat <- dat[["biocrates_data"]]

    metabolite_matrix <- dat[["raw_data"]][, .SD, .SDcols = !c("Plate Bar Code", "Sample Identification", "Sample Type")]

    HTML(paste0(
      "<h4> Data summary:</h4><br/>",
      "<b>Compounds:</b> ", dat[["n_cmp"]], ", <br/> ",
      "<b>Samples:</b> ", dat[["n_smp"]], ", <br/> ",
      "<b>Sample Types:</b> ",  paste0(unique(info_dat[, `Sample Type`]), collapse = ", "), ", <br/> ",
      "<b>QC levels:</b> ", uniqueN(info_dat[grepl("QC", `Sample Type`), `Sample Type`]), ", <br/> ",
      "<b> `< LOD` values:</b> ", sum(metabolite_matrix == "< LOD" & !is.na(metabolite_matrix)), ", <br/> ",
      "<b> `< LLOQ` values:</b> ", sum(metabolite_matrix == "< LLOQ" & !is.na(metabolite_matrix)), ", <br/> ",
      "<b> `> ULOQ` values:</b> ", sum(metabolite_matrix == "> ULOQ" & !is.na(metabolite_matrix)), ", <br/> ",
      "<b>Missing values:</b> ", sum(is.na(metabolite_matrix) | metabolite_matrix == "NA"), ", <br/> ",
      "<b>Infinite values (∞):</b> ", sum(metabolite_matrix == Inf & !is.na(metabolite_matrix)), ", <br/> ",
      "<b>Material: </b>", paste0(unique(info_dat[, Material]), collapse = ", "), ", <br/> ",
      "<b>OP: </b>", paste0(unique(info_dat[, OP]), collapse = ", "), ", <br/> ",
      "<b>Plate Bar Code: </b>", paste0(unique(info_dat[, `Plate Bar Code`]), collapse = ", "), ", <br/> ",
      "<b>Sample Volume: </b>", paste0(unique(info_dat[, `Sample Volume`]), collapse = ", "),
      "."
    ))
  })


  output[["biocrates_matrix"]] <- DT::renderDataTable({
    req(dat[["raw_data"]])

    custom_datatable(dat[["raw_data"]],
                     scrollY = 550,
                     paging = FALSE)
  })

  output[["biocrates_full_data"]] <- DT::renderDataTable({
    req(dat[["biocrates_data"]])
    custom_datatable(dat[["biocrates_data"]],
                     scrollY = 550,
                     paging = FALSE)
  })

  output[["LOD_table"]] <- DT::renderDataTable({
    req(dat[["LOD_table"]])
    custom_datatable(dat[["LOD_table"]],
                     scrollY = 550,
                     paging = FALSE)
  })


  observeEvent(input[["LOD_strategy"]], {
    req(dat[["raw_data"]])
    req(dat[["removed_LOD"]])

    LOD_frac <- switch (input[["LOD_strategy"]],
                        zero = 0,
                        `1/2` = 0.5,
                        `1/sqrt2` = sqrt(0.5),
                        `detection limit` = 1)
    browser()

    dat[["completed_data"]] <- metaboR:::handle_LOD(dat[["removed_LOD"]],
                                                    dat[["LOD_table"]],
                                                    LOD_frac = LOD_frac,
                                                    LOD_type = "calc")
    dat[["removed_CV"]] <- dat[["completed_data"]]
    dat[["CV_ratios_complete"]] <- attr(dat[["removed_CV"]], "CV_table")
    dat[["CV_ratios"]] <- attr(dat[["removed_CV"]], "CV_table")

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


  ################ Setup

  output[["group_columns"]] <- DT::renderDataTable({
    req(dat[["biocrates_data"]])
    custom_datatable(dat[["biocrates_data"]],
                     scrollY = 550,
                     paging = FALSE,
                     selection = list(mode = "single", target = "column"))
  })


  output[["selected_group"]] <- renderUI({
    req(dat[["biocrates_data"]])

    selected_col_id <- input[["group_columns_columns_selected"]]
    selected <- NULL
    if(is.null(selected_col_id)){
      selected <- "none"
    } else {
      column_names_dat <- colnames(dat[["biocrates_data"]])
      selected <- column_names_dat[selected_col_id]
      if(any(is.na(dat[["biocrates_data"]][`Sample Type` == "Sample", get(selected)]))) {
        showNotification(paste0("You cannot select ", selected, ". It contains missing values!"),
                         type = "error")
        selected <- "none"
      }
    }
    dat[["selected_group"]] <- selected
    HTML(paste0("Selected: ", selected))
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
    req(dat[["CV_ratios"]])

    dat[["removed_CV"]] <- metaboR:::remove_high_CV(dat[["removed_CV"]], get_to_remove(to_remove))
    dat[["CV_ratios"]] <- metaboR:::remove_high_CV(dat[["CV_ratios"]], get_to_remove(to_remove))

    QC_levels <- colnames(dat[["CV_ratios"]])[-1]
    updateSelectInput(session,
                      inputId = "QC_type",
                      choices = QC_levels,
                      selected = "QC Level 2")

    req(input[["QC_type"]])

    custom_datatable(setorderv(dat[["CV_ratios"]], input[["QC_type"]], order = -1),
                     scrollY = 550,
                     paging = FALSE)
  })

  to_remove_CV <- reactive({
    req(input[["CV_thresh"]])
    req(input[["QC_type"]])
    req(dat[["CV_ratios"]])

    metaboR:::get_CV_to_remove(dat[["removed_CV"]],
                               input[["CV_thresh"]],
                               QC_type = input[["QC_type"]],
                               dat[["CV_ratios"]])
  })


  output[["to_remove_CV_names"]] <- renderUI({
    get_remove_html_content(to_remove_CV())
  })


  observeEvent(input[["remove_CV_btn"]], {
    req(dat[["removed_CV"]])
    to_remove[["CV"]] <- to_remove_CV()

    if(length(to_remove_CV()) > 0) {
      dat[["removed_CV"]] <- metaboR:::remove_high_CV(dat[["removed_CV"]], to_remove_CV())
      dat[["CV_ratios"]] <- metaboR:::remove_high_CV(dat[["CV_ratios"]], get_to_remove(to_remove))
    }
  })


  observeEvent(input[["undo_CV_btn"]], {
    req(dat[["completed_data"]])
    req(dat[["removed_CV"]])

    to_remove[["CV"]] <- NULL
    dat[["removed_CV"]] <- copy(dat[["completed_data"]])
    dat[["CV_ratios"]] <- dat[["CV_ratios_complete"]]

    total_removing <- get_to_remove(to_remove)

    if(!is.null(total_removing)){
      dat[["removed_CV"]] <- metaboR:::remove_high_CV(dat[["removed_CV"]], total_removing)
      dat[["CV_ratios"]] <- metaboR:::remove_high_CV(dat[["CV_ratios"]], total_removing)
    }
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
      dat[["CV_ratios"]] <- metaboR:::remove_high_CV(dat[["CV_ratios"]], to_remove[["CV_hand"]])
    }
  })

  observeEvent(input[["undo_CV_hand_btn"]], {
    req(dat[["removed_CV"]])

    to_remove[["CV_hand"]] <- NULL
    dat[["removed_CV"]] <- copy(dat[["completed_data"]])
    dat[["CV_ratios"]] <- dat[["CV_ratios_complete"]]

    total_removing <- get_to_remove(to_remove)

    if(!is.null(total_removing)){
      dat[["removed_CV"]] <- metaboR:::remove_high_CV(dat[["removed_CV"]], total_removing)
      dat[["CV_ratios"]] <- metaboR:::remove_high_CV(dat[["CV_ratios"]], total_removing)
    }
  })

  output[["to_remove_CV_hand_names"]] <- renderUI({
    get_remove_html_content(unlist(to_remove_CV_by_hand()))
  })


  final_data <- reactive({
    req(dat[["removed_LOD"]])

    plt_data <- copy(dat[["removed_LOD"]])
    plt_data <- plt_data[`Sample Type` != "Sample",
                         `Sample Identification` := paste0(`Sample Identification`,
                                                           "_",
                                                           1:length(`Sample Identification`))]
    total_removing <- get_to_remove(to_remove)

    plt_data <- metaboR:::complete_LOD(plt_data, dat[["LOD_table"]])

    if(!is.null(total_removing)) {
      plt_data <- plt_data[!(Compound %in% total_removing)]
    }

    plt_data <- dcast(plt_data, `Sample Identification` + `Sample Type` ~ Compound, value.var = "Value")
    setnames(plt_data, old = "Sample Type", new = "SampleType", skip_absent = TRUE)

    plt_data
  })


  pca_res <- reactive({
    req(dat[["removed_LOD"]])

    plt_pca_dat <- final_data()

    list(pca_res = prcomp(plt_pca_dat[, -c(1:2)]),
         plot_data = plt_pca_dat)

  })



  output[["PCA_QC"]] <- renderPlot({
    pca_results <- pca_res()

    plt1 <- autoplot(pca_results[["pca_res"]], data = pca_results[["plot_data"]],
                     colour = "SampleType", shape = "SampleType") +
      theme_minimal() +
      scale_color_manual(values = c("#f2bb05", "#dd6e42", "#18bc9c", "#3e3f3a")) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0)

    exp_var_df <- data.frame(
      explained_variance_ratio = pca_results[["pca_res"]][["sdev"]]^2 /
        sum(pca_results[["pca_res"]][["sdev"]]^2),
      dimensions = as.factor(1:length(pca_results[["pca_res"]][["sdev"]]))
    )[1:10, ]

    plt2 <- ggplot(exp_var_df, aes(x = dimensions, y = explained_variance_ratio * 100)) +
      geom_col(fill = "#90bc9c") +
      geom_text(mapping = aes(x = dimensions,
                              y = explained_variance_ratio * 100 + 5,
                              label = paste0(round(explained_variance_ratio* 100, 2), "%"))) +
      theme_minimal() +
      ylab("variance explained (%)") +
      ggtitle("Scree plot") +
      xlab("Component number")

    plt2 + plt1 + plot_layout(widths = c(1, 2))

  })

  #### Download

  observeEvent(input[["app_panel"]], {
    if(input[["app_panel"]] == "download")
      updateNavbarPage(session, "main_navbar", selected = "Download")
  })


  output[["download_excel"]] <- downloadHandler(
    filename = function() { "results.xlsx"},
    content = function(file) {
      wb_file <- createWorkbook()

      # final data
      addWorksheet(wb_file, "Data")
      writeData(wb_file, "Data", final_data())

      # dat data
      dat_list <- reactiveValuesToList(dat)
      to_save <- c(setdiff(names(dat_list), c("samples_info")))

      for(sheet_name in to_save) {
        addWorksheet(wb_file, sheet_name)
        writeData(wb_file, sheet_name, dat[[sheet_name]])
      }

      #samples info
      samples_info <- list_to_dummy_frame(dat[["samples_info"]])
      addWorksheet(wb_file, "samples_info")
      writeData(wb_file, "samples_info",samples_info)

      # removing data
      to_remove_list <- reactiveValuesToList(to_remove)
      removal_setup <- list_to_dummy_frame(to_remove_list)

      addWorksheet(wb_file, "Removed")
      writeData(wb_file, "Removed", removal_setup)

      saveWorkbook(wb_file, paste0(file), overwrite = TRUE)
    }
  )

  output[["to_remove_total"]] <- renderUI({
    req(to_remove)

    to_remove_list <- reactiveValuesToList(to_remove)
    remove_types <- names(to_remove_list)

    HTML(do.call(
      paste0,
      lapply(remove_types, function(ith_type) {
        paste0(get_single_removal_content(to_remove_list, ith_type), "<br/><br/>")
      })
    ))

  })

}

shinyApp(ui, server)








