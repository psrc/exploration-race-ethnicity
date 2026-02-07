library(tidyverse)
library(openxlsx)
library(scales)
library(patchwork)
library(showtext)
library(psrcplot)
library(here)
library(forcats)

font_add_google("Poppins")
showtext_auto()

create_facet_bar_chart <- function(df, title, subtitle, x_val) {
  
  val_colname <- colnames(df)[colnames(df) %in% c('share', 'count', 'median')]

  df <- df |> 
    mutate({{val_colname}} := replace_na(.data[[val_colname]], 0))

  # create named vector to replace original y axis labels
  name_vec <- df |> 
    select(order, RACE) |> 
    mutate_at(c("order", "RACE"), as.character) |> 
    deframe()
  
  if(val_colname == 'share') {
    cc_lim <- c(0,1)
    plot_scale <- scale_x_continuous(labels = percent, expand = c(0,0))
  } else {
    max_val <- max(df[[val_colname]])
    # cc_lim <- c(0, ceiling(max_val) + 10000) #rentercostburden
    cc_lim <- c(0, ceiling(max_val) + 100000) #hhcount ownershiprate
    plot_scale <-scale_x_continuous(labels = comma, expand = c(0,0))
  }
  
  p <- ggplot() +
    geom_col(data = df, aes(y = fct_rev(.data[[x_val]]), x = .data[[val_colname]], fill = description),
             position = position_dodge(width = 0.9)) +
    geom_linerange(data = df, aes(y = fct_rev(.data[[x_val]]), xmin = lower, xmax = upper, group = description),
                   position = position_dodge(0.9)
                   ) +
    plot_scale +
    coord_cartesian(xlim = cc_lim) +
    scale_y_discrete(labels = function(x) str_wrap(name_vec[x], width = 20)) +
    scale_fill_discrete(palette = psrc_colors$pognbgy_5, name = str_to_title(val_colname), 
                        guide = guide_legend(reverse = TRUE)) +
    labs(title = title,
         subtitle = subtitle,
         x = NULL,
         y = NULL) +
    theme(plot.title = element_text(size = 28, margin = margin(t = 1, b = 0.5, unit = "cm")),
          plot.subtitle = element_text(size = 25, margin = margin(b = 1, unit = "cm")),
          axis.text.y = element_text(size = 17, lineheight = .5, vjust = .5),
          axis.text.x = element_text(size = 17),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(family = "Poppins"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "lightgrey", linetype = "solid", size = 0.5),
          panel.background = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.key.size = unit(.75, "cm"),
          legend.spacing = unit(1, "cm")
    )
  
  p_f <- p +
    facet_wrap(vars(facet), scales = "free") +
    theme(strip.text.x = element_text(size = 25, face = "bold"),
          strip.background = element_blank(),
          strip.text = element_text(margin = margin(b = 10, unit = "pt")),
          panel.spacing = unit(.5, "cm"),
          plot.margin = margin(10, 20, 0, 0, "pt"))
  
  p_f
}