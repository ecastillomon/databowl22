##Function taken from NGS higlight packages and modified
plot_field=function(field_color="#a4c3b2", line_color = "#ffffff") {
  field_height <- 160/3
  field_width <- 120

  field <- ggplot() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      plot.subtitle = element_text(hjust = 1),
      legend.position = "bottom",
      # legend.title = element_text(color = "#212529", size = 12, vjust = 1),
      legend.title.align = 1,
      # legend.text = element_text(color = "#343a40", size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      # panel.background = element_blank(),
      panel.background = element_rect(fill = field_color, color = "white"),
      panel.border = element_blank(),
      aspect.ratio = field_height/field_width
    ) +
    # major lines
    annotate(
      "segment",
      x = c(0, 0, 0,field_width, seq(10, 110, by=5)),
      xend = c(field_width,field_width, 0, field_width, seq(10, 110, by=5)),
      y = c(0, field_height, 0, 0, rep(0, 21)),
      yend = c(0, field_height, field_height, field_height, rep(field_height, 21)),
      colour = line_color
    ) +
    # hashmarks
    annotate(
      "segment",
      x = rep(seq(10, 110, by=1), 4),
      xend = rep(seq(10, 110, by=1), 4),
      y = c(rep(0, 101), rep(field_height-1, 101), rep(160/6 + 18.5/6, 101), rep(160/6 - 18.5/6, 101)),
      yend = c(rep(1, 101), rep(field_height, 101), rep(160/6 + 18.5/6 + 1, 101), rep(160/6 - 18.5/6 - 1, 101)),
      colour = line_color
    ) +
    # yard numbers
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      size = 7,
      family = "mono",
      colour = line_color, # "#495057",
    ) +
    # yard numbers upside down
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(field_height-12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      angle = 180,
      size = 7,
      family = "mono",
      colour = line_color,
    )

  return(field)
}


# df_plays %>%
#   distinct(event) %>%
#   collect() ->events
