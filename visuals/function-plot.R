library(tidyverse)
library(openxlsx)
library(scales)
library(patchwork)
library(showtext)
library(psrcplot)
library(here)

font_add_google("Poppins")
showtext_auto()

create_bar_chart <- function(df, title) {
  ggplot() +
    # geom_col(data = df, aes(x = RACE, y = median, fill = str_extract(description, "^.\\w+(?=_)")),
    geom_col(data = df, aes(x = RACE, y = median, fill = description),
             position = position_dodge(width = 0.9)) +
    geom_linerange(data = df, aes(x = RACE, ymin = lower, ymax = upper, group = description),
                   position = position_dodge(0.9)) +
    coord_cartesian(ylim = c(0, 250000)) +
    scale_y_continuous(labels = comma, expand = c(0,0)) +
    scale_x_discrete(labels = label_wrap(width = 15)) +
    scale_fill_discrete(palette = psrc_colors$pognbgy_5, name = "Median HINCP") +
    labs(title = title,
         x = NULL,
         y = NULL) +
    theme(plot.title = element_text(size = 22, margin = margin(t = 1, b = 1, unit = "cm")),
          axis.text.x = element_text(size = 17, lineheight = .5, vjust = 0),
          # axis.text.x = element_text(size = 17, angle = 45, vjust = .9, hjust = 1),
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
          # axis.line.y = element_line(colour = "gray")
    )
}