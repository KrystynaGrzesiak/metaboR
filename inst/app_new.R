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



ui <- navbarPage(
  title = "Metabocrates",
  theme = shinytheme("flatly"),

  tabPanel("About", ui_content_about()),
  tabPanel("Run",
           display_navigation_bar(
             steps = c("Upload data",
                       "Remove metabolites with high LOD proportion",
                       "Complete LOD",
                       "Quality control",
                       "Summary statistics",
                       "Download")
           ),

           tabsetPanel(
             id = "app_panel",
             type = "hidden",
             tabPanel(
               "upload_data",
               column(3,
                      style = "background-color:#F6FBFC; border-right: 1px solid",
                      br(),
                      br(),
                      br(),
                      h3("Upload new data"),
                      fileInput(
                        inputId = 'new_data_path',
                        label = "Upload Biocrates® file.",
                        multiple = FALSE,
                        accept = c(".xlsx", ".xls")
                      ),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      h3("... or load your previous project"),
                      fileInput(
                        inputId = 'project_path',
                        label = "Upload Excel sheet downloaded from MetaboCrates.",
                        multiple = FALSE,
                        accept = c(".xlsx", ".xls")
                      ),
                      br(),
                      br(),
                      br(),
               ),
               column(7,
                      offset = 1,
                      tabsetPanel(
                        tabPanel(
                          "Data summary",
                          htmlOutput("samples_info"),
                          br(),
                          girafeOutput("samples_info_plt", width = "1000px", height = "400px" )
                        ),
                        tabPanel(
                          "Dataset preview",
                          h3("You can see metabolomics matrix and LOD table below:"),
                          tabsetPanel(
                            tabPanel("Metabolites",
                                     withSpinner(DT::dataTableOutput("biocrates_data"))),
                            tabPanel("LOD values",  withSpinner(DT::dataTableOutput("LOD_table")))
                          )
                        )
                      )


               )
             ),
             tabPanel("upload_data")
           )
  ),
  tabPanel("Download")
)

server <- function(input, output, session) {

  dat <- reactiveValues()

  ##### loading data
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
  })

  ## info
  output[["samples_info"]] <- renderUI({
    req(dat[["raw_data"]])
    req(dat[["n_smp"]])
    req(dat[["n_cmp"]])

    info <- attr(dat[["raw_data"]], "samples_info")
    content <- paste0(
      "<h3> Study informations:</h3><br/>",
      "<b>Compounds:</b> ", dat[["n_cmp"]], ", <br/> ",
      "<b>Samples:</b> ", dat[["n_smp"]], ", <br/> ",
      "<b>QC samples:</b> ", length(info[["QC_levels"]]), ", <br/> ",
      "<b>QC levels:</b> ", uniqueN(info[["QC_levels"]]), ", <br/> ",
      "<b>Species: </b>", paste0(info[["species"]], collapse = ", "), ", <br/> ",
      "<b>OP: </b>", paste0(info[["OP"]], collapse = ", "), ", <br/> ",
      "<b>Material: </b>", paste0(info[["Material"]], collapse = ", "), ", <br/> ",
      "<b>Sample Volume: </b>", paste0(info[["Sample_Volume"]], collapse = ", "), "."
    )
    HTML(content)
  })

  output[["samples_info_plt"]] <- renderGirafe({
    req(dat[["raw_data"]])
    # browser()
    info <- attr(dat[["raw_data"]], "samples_info")

    sub_name <- data.table(`Submission Names` = info[["Submission_Names"]])[
      , .N, by = .(`Submission Names`)
    ]

    sample_type <- data.table(`Sample Type` = info[["sample_type"]])[
      !is.na(`Sample Type`), .N, by = .(`Sample Type`)
    ]

    gender <- data.table(`gender` = info[["gender"]])[
      !is.na(gender), .N, by = .(`gender`)
    ]

    p1 <- ggplot(sub_name, aes(x = `Submission Names`, y = N, tooltip = N)) +
      geom_bar_interactive(stat = "identity", fill = "#95a5a6") +
      theme_minimal() +
      ylab("N") +
      theme(text = element_text(size = 18),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(margin = margin(t = 20)))


    p2 <- ggplot(sample_type, aes(x = `Sample Type`, y = N, tooltip = N)) +
      geom_bar_interactive(stat = "identity", fill = "#18bc9c") +
      theme_minimal() +
      ylab("") +
      theme(text = element_text(size = 18),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(margin = margin(t = 20)))

    p3 <- ggplot(gender, aes(x = `gender`, y = N, tooltip = N)) +
      geom_bar_interactive(stat = "identity", fill = "#90bc9c") +
      theme_minimal() +
      ylab("") +
      theme(text = element_text(size = 18),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(margin = margin(t = 20)))

    plt <- p1 + p2 + p3 +  plot_layout(widths = c(1, 2, 1))

    girafe(code = print(plt),
           options = list(opts_sizing(rescale = FALSE)),
           width_svg = 14,
           height_svg = 6)


  })


  ## Data preview
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



}

shinyApp(ui, server)








