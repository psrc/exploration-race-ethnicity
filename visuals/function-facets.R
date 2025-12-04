library(tidyverse)
library(openxlsx)
library(scales)
library(patchwork)
library(showtext)
library(psrcplot)
library(here)

font_add_google("Poppins")
showtext_auto()

create_facet_chart <- function(df, title, subtitle, x_val) {

  val_colname <- colnames(df)[colnames(df) %in% c('share', 'count', 'median')]
  # browser()
  df <- df |> 
    mutate({{val_colname}} := replace_na(.data[[val_colname]], 0))
  
  if(val_colname == 'share') {
    cc_lim <- c(0,1)
    plot_scale <- scale_y_continuous(labels = percent, expand = c(0,0))
  } else {
    max_val <- max(df[[val_colname]])
    # browser()
    cc_lim <- c(0, ceiling(max_val) + 100000)
    plot_scale <-scale_y_continuous(labels = comma, expand = c(0,0))
  }
  
  p <- ggplot() +
    geom_col(data = df, aes(x = .data[[x_val]], y = .data[[val_colname]], fill = description),
             position = position_dodge(width = 0.9)) +
    geom_linerange(data = df, aes(x = RACE, ymin = lower, ymax = upper, group = description),
                   position = position_dodge(0.9)) +
    plot_scale +
    coord_cartesian(ylim = cc_lim) +
    scale_x_discrete(labels = label_wrap(width = 20)) +
    # scale_x_discrete(labels = label_wrap(width = 15)) +
    scale_fill_discrete(palette = psrc_colors$pognbgy_5, name = str_to_title(val_colname)) +
    labs(title = title,
         subtitle = subtitle,
         x = NULL,
         y = NULL) +
    theme(plot.title = element_text(size = 28, margin = margin(t = 1, b = 0.5, unit = "cm")),
          plot.subtitle = element_text(size = 25, margin = margin(b = 1, unit = "cm")),
          axis.text.x = element_text(margin = margin(t = 10, unit = "pt"), size = 17, angle = 90, lineheight = .5, vjust = 0),
          # axis.text.x = element_text(size = 17, angle = 90, lineheight = 0, vjust = 0),
          axis.text.y = element_text(size = 17),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(family = "Poppins"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(color = "lightgrey", linetype = "solid", size = 0.5),
          panel.background = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.key.size = unit(.75, "cm"),
          legend.spacing = unit(1, "cm")
    )
  # browser()
  p +
    facet_wrap(vars(facets), scales = "free_x") +
    theme(strip.text.x = element_text(size = 25, face = "bold"),
          strip.background = element_blank(),
          strip.text = element_text(margin = margin(b = 10, unit = "pt")),
          panel.spacing = unit(.5, "cm"),
          plot.margin = margin(10, 0, 0, 0, "pt"))
}