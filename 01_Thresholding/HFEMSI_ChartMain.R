
write_rds(unnested_work_summary, "HFEMSI_MinsChart.rds")

library(ggplot2)
library(tidyverse)

unnested_work_summary = read_rds("HFEMSI_MinsChart.rds")

ggplot(unnested_work_summary, aes(day_index, timebelow775)) + 
  geom_col(aes(fill = lowest.pred), width = 1, colour = "black") + 
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  facet_wrap(~filename) +
  labs(x = "Day into Mission", y = "Daily Minutes Working Below 77", fill = "Minimum Value")

