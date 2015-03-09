#' Nicer default theme for ggplot2
#' 
#' @export
theme_plain <-  function () {
  theme(
    text = element_text(family="mono"),
    plot.title = element_text(family="sans", size = 16, vjust = 1.5),
    axis.title.x = element_text(family="sans",vjust=-0.25),
    axis.title.y = element_text(family="sans"),
    legend.background = element_rect(fill = "white"),
    #legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "#e5e5e5"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#efefef", colour = NA),
    strip.background = element_rect(fill = "#444444", colour = NA),
    strip.text = element_text(face="bold", colour = "white")
  )    
}

#' Empty theme
#' @export
theme_empty <- function () {
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
}