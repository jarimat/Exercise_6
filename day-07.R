# Jake Matullo
# 2/17/2025
# To practice with ggplots to make some cool COVID graphs.

# Question 1

top_6_states <- covid |>
  filter(date == max(date)) |>
  group_by(state) |>
  summarize(total_cases = sum(cases, na.rm = TRUE)) |>
  slice_max(order_by = total_cases, n =6) %>%
  pull(state)

print(top_6_states)

covid_filter <- covid %>%
  filter(state %in% top_6_states) %>%
  group_by(state, date) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
  arrange(state, date)


p <- ggplot(covid_filter, aes(x = date, y = total_cases, color = state)) +
  geom_smooth(size = 1.2) +
  facet_wrap(~ state, scales = "free_y") +
  labs(title = "COVID-19 Cases in Top 6 States", x = "Date", y = "Cases") +
  theme_minimal()


print(p)

ggsave("img/covid_cases_in_top_6_states.png", plot = p, width = 10, height = 5, dpi = 350)

# Question 2

library(dplyr)
daily_cases <- covid %>%
  +     group_by(date) %>%
  +     summarise(total_cases = sum(cases, na.rm = TRUE))
View(daily_cases)
library(ggplot2)

ggplot(daily_cases, aes(x = date, y = total_cases)) +
  geom_col(fill = "skyblue") +
  labs(
    title = "Daily Total COVID-19 Cases in the USA",
    x = "Date",
    y = "Total Cases"
  ) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("img/daily_cases_plot.png", width = 10, height = 6)
