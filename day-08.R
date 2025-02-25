# Jake Matullo
# 2/21/2025
# To do Exercise 8.

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

df = data.frame(region = state.region,
                state = state.name)

inner_join(df, covid, by = "state") |>
  group_by(region, date) |>
  summarize(cases = sum(cases),
            deaths = sum(deaths)) |>
  pivot_longer(cols = c(cases, deaths),
               names_to = "type",
               values_to = "count") |>
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  facet_grid(type~region, scales = "free_y")+
  theme_bw()
