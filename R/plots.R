#' Draws plots based on raw data
#'
#' @description This function plots Submission Names, `Sample Type and Gender on
#' three elegant bar plots.
#'
#' @param LOD_data an object of metaboR_LOD_data class.
#'
#' @details This function returns an ggplot2 object.
#'
#' @export plot_raw_info
#'

plot_raw_info <- function(LOD_data) {

  info <- attr(LOD_data, "samples_info")

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
    theme(text = element_text(size = 16),
          axis.text = element_text(size = 10),
          axis.title.x = element_text(margin = margin(t = 20)))


  p2 <- ggplot(sample_type, aes(x = `Sample Type`, y = N, tooltip = N)) +
    geom_bar_interactive(stat = "identity", fill = "#18bc9c") +
    theme_minimal() +
    ylab("") +
    theme(text = element_text(size = 16),
          axis.text = element_text(size = 10),
          axis.title.x = element_text(margin = margin(t = 20)))

  p3 <- ggplot(gender, aes(x = `gender`, y = N, tooltip = N)) +
    geom_bar_interactive(stat = "identity", fill = "#90bc9c") +
    theme_minimal() +
    ylab("") +
    theme(text = element_text(size = 16),
          axis.text = element_text(size = 10),
          axis.title.x = element_text(margin = margin(t = 20)))

  plt <- p1 + p2 + p3 +  plot_layout(widths = c(1, 2, 1))
}









plot_LOD_proportion <- function(metaboR_LOD_data) {

  LOD_ratios <- metaboR_LOD_data[
    ,.SD, .SDcols = !c('Plate Bar Code', 'Sample Type', 'Sample_ID')
  ][, lapply(.SD, function(ith_col) {
    mean(ith_col == "< LOD", na.rm = TRUE)
  })]

  LOD_ratios <- melt(LOD_ratios, measure.vars = colnames(LOD_ratios))

  setnames(LOD_ratios, new = c("Compound", "< LOD ratio"), skip_absent = TRUE)
  setorder(LOD_ratios, "< LOD ratio")

  LOD_ratios[
    , above_limit := `< LOD ratio` > 0.2
  ][
    , colors_legend := ifelse(above_limit, "tomato", "black")
  ]

  LOD_ratios <- LOD_ratios[ `< LOD ratio` > 0]

  level_order <- LOD_ratios[, Compound]

  ggplot(LOD_ratios, Compound = factor(Compound, levels = Compound)) +
    geom_segment(aes(y = Compound, yend = Compound,
                     x = 0, xend = `< LOD ratio`, col = above_limit),
                 size = 0.5) +
    scale_color_manual(values = sort(unique(LOD_ratios[, colors_legend]))) +
    geom_segment(aes(y = Compound, yend = Compound,
                     xend = 1, x = `< LOD ratio`),
                 size = 0.5, col = "grey") +
    # geom_point(mapping = aes(y = Compound, x = `< LOD ratio`), size = 2) +
    geom_label(mapping = aes(y = Compound, x = `< LOD ratio`,
                             label = round(`< LOD ratio`, 3)),
               size = 3) +
    theme_minimal() +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 11),
          title = element_text(size = 13),
          legend.position = "none") +
    ggtitle("< LOD values ratio") +
    scale_y_discrete(limits = level_order) +
    xlab("<LOD ratio")
}




