install.packages("easypackages")
library("easypackages")
libraries("readr", "data.table", "dplyr", "tidyr", "gtsummary","openxlsx", "readxl", "MASS", "ggplot2", "forcats", "hrbthemes", "viridis")



edges %>% 
  pivot_longer(!Real_random, names_to = "type", values_to = "value") %>%
  ggplot( aes(x=Real_random, y=value, fill=type)) +
    geom_violin()
