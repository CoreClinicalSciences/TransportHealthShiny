library(ggplot2)
library(ggtext)

CCS_theme <- function(scale_type = NULL, reverse = F) {
  font <- "Arial"
  
  CCS_color <- c("#073660", "#1C7986", "#2FB9AB", "#EFC705", "#F04F24", "#942092", "#521B92")
  #extended_CCS_color <- c(CCS_color, "#C62828", "#6A1B9A","#00695C", "#EF6C00", "#283593", "#1565C0")
  
    
    if (is.character(scale_type)) {
      scale_type <- match.arg(scale_type, c("fill", "color"))  
      
      if (scale_type == "fill") {
        color_scale <- scale_fill_manual(values = CCS_color)
      } else {
        color_scale <- scale_color_manual(values = CCS_color)
      } 
    } else {color_scale <- NULL}
    
   ### *** Pending: needs to consider the relationship between subgroup and scale_type
  
  
  # if (!is.null(data) && !is.null(subgroup)) {
  #   unique_subgroups <- unique(data[[subgroup]])
  
  
  theme <- ggplot2::theme(
    # Text format:
    # This sets the font, size, type and colour of text for the chart's title
    plot.title = ggtext::element_markdown(family = font,
                                          size = 28,
                                          face = "bold",
                                          color = "#073660"),
    # plot.title = ggplot2::element_text(family = font,
    #                                    size = 28,
    #                                    face = "bold",
    #                                    color = "#073660"),
    # This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggtext::element_markdown(family = font,
                                             size = 22,
                                             color = "#1C7986",
                                             margin = ggplot2::margin(7, 0, 7, 0)),
    plot.caption = ggtext::element_markdown(family = font,
                                            size = 12,
                                            color = "black",
                                            margin = ggplot2::margin(7, 0, 7, 0)),
    # This leaves the caption text element empty, because it is set elsewhere in the finalise_plot()
    
    # Legend format
    # This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. 
    # The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    #legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggtext::element_textbox(family = font,
                                          size = 18,
                                          color = "black",
                                          hjust = 0),
    
    # Axis format
    # This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. 
    # In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so
    axis.line = ggplot2::element_line(color = "black", linewidth = 1.5),
    axis.title.x = ggtext::element_markdown(family = font,
                                            size = 22,
                                            color = "black"),
    axis.title.y = ggtext::element_markdown(family = font,
                                            size = 22,
                                            color = "black"),
    axis.text = ggtext::element_markdown(family = font,
                                         size = 18,
                                         color = "black"),
    axis.text.x = ggtext::element_markdown(margin = ggplot2::margin(5, b = 10)),
    axis.text.y = ggtext::element_markdown(margin = ggplot2::margin(5, r = 10)),
    
    # Grid lines
    # This removes all minor gridlines. We can add major x or y gridlines. 
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(), #ggplot2::element_line(color = "#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    # Blank background
    # This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    # Strip background 
    # This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour 
    # and sets the title size of the facet-wrap title to font size 22
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggtext::element_markdown(size  = 22,  hjust = 0)
  )
  
  if (!is.null(color_scale)) {
    list(theme, color_scale)
  } else {
    list(theme)
  }
  
}