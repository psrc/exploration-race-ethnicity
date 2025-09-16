library(tidyverse)
library(openxlsx)
library(scales)
library(patchwork)
library(showtext)
library(psrcplot)

font_add_google("Poppins")
showtext_auto()

create_bar_chart <- function(df, title) {
  ggplot() +
    geom_col(data = df, aes(x = RACE, y = median, fill = description),
             position = position_dodge(width = 0.9)) +
    geom_linerange(data = df, aes(x = RACE, ymin = lower, ymax = upper, group = description),
                   position = position_dodge(0.9)) +
    scale_y_continuous(labels = comma, expand = c(0,0)) +
    scale_x_discrete(labels = label_wrap(width = 25)) +
    scale_fill_discrete(palette = psrc_colors$pognbgy_5) +
    labs(title = title,
         x = NULL,
         y = NULL) +
    theme(legend.title=element_blank(),
          axis.text.x = element_text(size = 8, angle = 45, vjust = .9, hjust = 1),
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
  mutate(tab_name = paste(first, second, third, sep = "_"))

# df <- read.xlsx("visuals/median-income-by-re-hhsize-tenure-with-differences.xlsx",
#           sheet = df_e$tab_name[[1]])

all_plots <- list()

for(t in df_e$tab_name) {
  df <- read.xlsx("visuals/median-income-by-re-hhsize-tenure-with-differences.xlsx",
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
  
  p1 <- create_bar_chart(df = df, title = paste(t, ", Part 1"))
  
  # p1
  
  df2 <- df_long |>
    filter(RACE %in% totals) |>
    mutate(upper = median + moe,
           lower = median - moe)
  p2 <- create_bar_chart(df = df2, title = paste(t, ", Part 2"))
  
  # p2
  
  all_p <- p1 / p2 + plot_layout(guides = 'collect')
  all_plots[[t]] <- all_p
}

# r <- df |> 
#   filter(COUNTY == "Region") |> 
#   select(RACE, ends_with("median"), ends_with("moe")) 
# 
# df_long <- r |> 
#   pivot_longer(cols = -RACE,
#                names_to = "variable",
#                values_to = "value"
#   ) |> 
#   mutate(
#     # Create median/moe indicator
#     type = if_else(str_detect(variable, "_moe$"), "moe", "median"),
#     
#     # Clean variable to just the description
#     description = str_remove(variable, "_moe$"),
#     description = str_replace(description, "_HINCP_median", "_HINCP")
#   ) |> 
#   select(RACE, description, type, value) |> 
#   pivot_wider(names_from = type,
#               values_from = value
#   )
# 
# categories <- unique(df_long$RACE)
# totals <- categories[grepl("^(To.*|Multi.*)", categories)]
# races <- setdiff(categories, totals)
# 
# df <- df_long |> 
#   filter(RACE %in% races) |> 
#   mutate(upper = median + moe,
#          lower = median - moe)
# 
# p1 <- create_bar_chart(df = df, title = "Add title")
# 
# p1
# 
# df2 <- df_long |>
#   filter(RACE %in% totals) |>
#   mutate(upper = median + moe,
#          lower = median - moe)
# p2 <- create_bar_chart(df = df2, title = "Add title")
# 
# p2
# 
# all_p <- p1 / p2 + plot_layout(guides = 'collect')
# all_p
