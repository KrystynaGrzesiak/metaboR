library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)

library(metaboR)

library(readxl)
library(openxlsx)

library(data.table)
library(DT)

source("supp/ui_supp.R")



ui <- navbarPage(
  title = "Metabocrates",
  theme = shinytheme("flatly"),

  tabPanel("About",
           tags$footer(
             align = "right",
             style = "position:absolute; bottom:0; width:99%; height:30px; padding: 110px;",
             HTML("<img src='logo.png' style='height: 100px'>"),
           ),
           ui_content_about()
  ),
  tabPanel("Run",
           display_navigation_bar(steps = c("Upload data",
                                            "Remove metabolites with high LOD proportion",
                                            "Complete LOD",
                                            "Quality control",
                                            "Summary statistics",
                                            "Download"))
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)








