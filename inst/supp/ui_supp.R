


display_bar <- function(steps) {
  tagList(
    HTML(
      paste0(
        '<div class="container">
         <div class="wrapper">
         <div class="arrow-steps clearfix">',
        '<div class="step current"> <span> ', steps[1],' </span> </div>',
        paste0('<div class="step"> <span> ', steps[-1], ' </span> </div>', collapse = " "),
        '</div>
         </div>
         </div>')
    )
  )
}


display_navigation_bar <- function(steps) {
  tagList(
    includeCSS("www/navigation.css"),
    tags$script(src = "navigation.js"),
    tags$footer(
      align = "center",
      style = "position:absolute; bottom:0; width:95%; height:30px; color: white; padding: 100px; z-index: 1000;",
      column(
        1,
        align = "center",
        HTML('<div class="nav clearfix"> <a type="button" id="prev" class="prev">&laquo; Previous</a> </div>')
      ),
      column(
        10,
        align = "center",
        display_bar(steps)
      ),
      column(
        1,
        align = "center",
        HTML('<div class="nav clearfix"> <a type="button" id="next" class="next">Next &raquo;</a> </div>')
      )
    )
  )
}



ui_content_about <- function() {
  tagList(
    fluidRow(
      column(1,
             align = "center",
             HTML("<img src='logo-placeholder.png' height='140px'>"),
      ),
      column(11,
             h2("Welcome to MetaboCrates!"),
             h3("Solution for early analysis of data from Biocrates® kits."),
      ),
    ),
    HTML('<hr style="border-color: black;">'),
    h3("Key Features:"),
    h4(HTML("<b> 1. Seamless Integration:</b> Easily upload your Biocrates® data files.")),
    h4(HTML("<b> 2. Data Cleaning:</b> Remove metabolites with a high Limit of Detection (LOD) proportion to ensure data accuracy.")),
    h4(HTML("<b> 3. Complete LOD:</b> Complete missing data points based on LOD values, making your dataset more comprehensive.")),
    h4(HTML("<b> 4. Quality Control:</b> Implement quality control checks to detect and handle outliers or anomalies for reliable data.")),
    h4(HTML("<b> 5. Summary Statistics:</b> Generate summary statistics and visualizations to quickly understand your metabolomics data.")),
    h4(HTML("<b> 5. Download Results:</b> Download processed data, summary statistics, and share with colleagues.")),
    h4(HTML("<b> 5. Save Your Progress:</b> Save your work to resume analysis at a later time or manage multiple projects effortlessly.")),
    h4(HTML("<b> 8. Secure and Confidential:</b> Your data privacy is our top priority."))
  )
}
