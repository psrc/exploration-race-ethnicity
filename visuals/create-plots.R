library(tidyverse)
library(openxlsx)
library(scales)
library(patchwork)
library(showtext)
library(psrcplot)
library(here)

datafile <- here("visuals/median-income-by-re-hhsize-tenure-with-differences.xlsx")

font_add_google("Poppins")
showtext_auto()

create_bar_chart <- function(df, title) {
  ggplot() +
    geom_col(data = df, aes(x = RACE, y = median, fill = str_extract(description, "^.\\w+(?=_)")),
             position = position_dodge(width = 0.9)) +
    geom_linerange(data = df, aes(x = RACE, ymin = lower, ymax = upper, group = description),
                   position = position_dodge(0.9)) +
    scale_y_continuous(labels = comma, expand = c(0,0)) +
    scale_x_discrete(labels = label_wrap(width = 25)) +
    scale_fill_discrete(palette = psrc_colors$pognbgy_5, name = "HINCP") +
    labs(title = title,
         # subtitle = "HINCP",
         x = NULL,
         y = NULL) +
    theme(plot.title = element_text(size = 22),
          axis.text.x = element_text(size = 17, angle = 45, vjust = .9, hjust = 1),
          axis.text.y = element_text(size = 17),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(family = "Poppins"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          # axis.line.y = element_line(colour = "gray")
          )
}

e <- data.frame(first = c("detail", "dichot"), second = c("mp", "sp"), third = c("own", "rent"))
df_e <- expand.grid(e) |> 
  mutate(tab_name = paste(first, second, third, sep = "_")) |> 
  mutate(second_text = case_when(second == "mp" ~ "Multi-Person",
                                 second == "sp" ~ "Single-Person")) |> 
  mutate(plot_name = str_to_title(paste(first, second_text, third)))

all_plots <- list()

for(t in df_e$tab_name) {
  
  chart_name <- df_e |> filter(tab_name == t) |> pull(plot_name)
  
  df <- read.xlsx(datafile,
                  sheet = t)
  
  r <- df |> 
    filter(COUNTY == "Region") |> 
    select(RACE, ends_with("median"), ends_with("moe")) 
  
  df_long <- r |> 
    pivot_longer(cols = -RACE,
                 names_to = "variable",
                 values_to = "value"
    ) |> 
    mutate(
      # Create median/moe indicator
      type = if_else(str_detect(variable, "_moe$"), "moe", "median"),
      
      # Clean variable to just the description
      description = str_remove(variable, "_moe$"),
      description = str_replace(description, "_HINCP_median", "_HINCP")
    ) |> 
    select(RACE, description, type, value) |> 
    pivot_wider(names_from = type,
                values_from = value
    )
  
  categories <- unique(df_long$RACE)
  totals <- categories[grepl("^(To.*|Multi.*)", categories)]
  races <- setdiff(categories, totals)
  
  df <- df_long |> 
    filter(RACE %in% races) |> 
    mutate(upper = median + moe,
           lower = median - moe)
  
  p1 <- create_bar_chart(df = df, title = paste(chart_name, ", Part 1"))
  
  df2 <- df_long |>
    filter(RACE %in% totals) |>
    mutate(upper = median + moe,
           lower = median - moe)
  p2 <- create_bar_chart(df = df2, title = paste(chart_name, ", Part 2"))
  
  
  all_p <- p1 / p2 + plot_layout(guides = 'collect')
  all_plots[[t]] <- all_p
}

all_plots