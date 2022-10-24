


metaboR_data <- function() {

}


new_metaboR_data <- function(dat,
                             type){

  type <- match.arg(type, c("untargeted_LC", "untargeted_GC", "targeted_LC",
                            "targeted_GC", "Biocrates"))

  structure(dat,
            class = c("metaboR_data", "data.frame"),
            type = type)
}
