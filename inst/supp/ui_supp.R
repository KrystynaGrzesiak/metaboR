

display_bar <- function(steps) {
  tagList(HTML(paste0(
    '<div class="container">
         <div class="wrapper">
         <div class="arrow-steps clearfix">',
    '<div class="step current"> <span> ', steps[1],' </span> </div>',
    paste0('<div class="step"> <span> ',
           steps[-1],
           ' </span> </div>',
           collapse = " "),
    '</div>
         </div>
         </div>')))
}


display_navigation_bar <- function(steps) {
  tagList(
    includeCSS("www/navigation.css"),
    tags$script(src = "navigation.js"),
    tags$footer(
      align = "center",
      style = "position:absolute; bottom:0; width:95%; height:30px; color: white; padding: 60px; z-index: 1000;",
      fluidRow(column(1, align = "center",
                      tags$button(id = "prev",
                                  type="button",
                                  class="btn action-button prev",
                                  HTML('<a type="button">Previous  &raquo;</a>'))),
               column(10, align = "center", display_bar(steps)),
               column(1, align = "center",
                      tags$button(id = "next",
                                  type="button",
                                  class="btn action-button next",
                                  HTML('<a type="button">Next &raquo;</a>'))))
    )
  )
}



ui_content_about <- function() {
  tagList(
    tags$footer(
      align = "right",
      style = "position:absolute; bottom:0; width:99%; height:30px; padding: 0px 0px 100px 100px;",
      HTML("<img src='logo.png' style='height: 100px'>"),
    ),
    fluidRow(column(1,
                    align = "center",
                    HTML("<img src='logo-placeholder.png' height='110px'>")),
             column(11,
                    h3("Welcome to MetaboCrates!"),
                    h4("Solution for early analysis of data from Biocrates® kits."))),
    HTML('<hr style="border-color: black;">'),
    h4("Key Features:"),
    h5(HTML("<b> 1. Seamless Integration:</b> Easily upload your Biocrates® data files.")),
    h5(HTML("<b> 2. Data Cleaning:</b> Remove metabolites with a high Limit of Detection (LOD) proportion to ensure data accuracy.")),
    h5(HTML("<b> 3. Complete LOD:</b> Complete missing data points based on LOD values, making your dataset more comprehensive.")),
    h5(HTML("<b> 4. Quality Control:</b> Implement quality control checks to detect and handle outliers or anomalies for reliable data.")),
    h5(HTML("<b> 5. Summary Statistics:</b> Generate summary statistics and visualizations to quickly understand your metabolomics data.")),
    h5(HTML("<b> 5. Download Results:</b> Download processed data, summary statistics, and share with colleagues.")),
    h5(HTML("<b> 5. Save Your Progress:</b> Save your work to resume analysis at a later time or manage multiple projects effortlessly.")),
    h5(HTML("<b> 8. Secure and Confidential:</b> Your data privacy is our top priority.")),
    HTML('<hr style="border-color: black;">'),
    h4("Contact:"),
    h5("Words, words, words..."),
    h4("How to cite:"),
    h5("Citation tratata"),
    h4("Funding and acknowledgements:"),
    h5("We want to thank the Clinical Research Centre (Medical University of
       Białystok) members for fruitful discussions. K.G. wants to acknowledge
       grant no. 2021/43/O/ST6/02805 (National Science Centre). M.C. acknowledges
       grant no. B.SUB.23.533 (Medical University of Białystok). The study was
       supported by the Ministry of Education and Science funds within the project
       'Excellence Initiative - Research University'. We also acknowledge the Center
       for Artificial Intelligence at the Medical University of Białystok (funded
       by the Ministry of Health of the Republic of Poland).")
  )
}


get_raw_html_content <- function(info, n_smp, n_cmp)
  HTML(paste0(
    "<h4> Study informations:</h4><br/>",
    "<b>Compounds:</b> ", n_cmp, ", <br/> ",
    "<b>Samples:</b> ", n_smp, ", <br/> ",
    "<b>QC samples:</b> ", length(info[["QC_levels"]]), ", <br/> ",
    "<b>QC levels:</b> ", uniqueN(info[["QC_levels"]]), ", <br/> ",
    "<b>Species: </b>", paste0(info[["species"]], collapse = ", "), ", <br/> ",
    "<b>OP: </b>", paste0(info[["OP"]], collapse = ", "), ", <br/> ",
    "<b>Material: </b>", paste0(info[["Material"]], collapse = ", "), ", <br/> ",
    "<b>Sample Volume: </b>", paste0(info[["Sample_Volume"]], collapse = ", "), "."
  ))


get_remove_html_content <- function(to_remove)
  HTML(ifelse(length(to_remove) > 0,
              paste0(paste(to_remove, collapse = ", "),
                     "<br/><br/> Total: ",
                     length(to_remove),
                     " metabolites."),
              "none"))
