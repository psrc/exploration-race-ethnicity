library(tidyverse)

file_names <- c("total-median-df.rds", "non-total-medians-df.rds")

# compile into one df ----

df <- map(file_names, ~readRDS(file.path("median-income/data/", .x))) |> 
  bind_rows()


