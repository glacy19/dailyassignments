Genesis Lacy
February 18, 2025 
ESS 330 Daily Exercise 7

library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url)


Question 1: 
  
  covid |>
    filter(date == max(date)) |>
    group_by(state) |>
    summarize(cases = sum(cases, na.rm = TRUE)) |>
    ungroup() |>
    slice_max(cases, n = 6) |>
    pull(state) ->
    top_states
  
  covid |>
    filter(state %in% top_states) |>
    group_by(state, date) |>
    summarize(cases = sum(cases)) |>
    ungroup() |>
    ggplot(aes(x = date, y = cases, color = state)) +
    geom_line(size = 2) +
    facet_wrap(~state) +
    theme_minimal() +
    theme(legend.position = 'NA') +
    labs(title = "Total Case Counts",
         subtitle = "Data Source: NY-Times",
         x = "Date",
         y = "Cases",
         caption = "Daily Exercise 07")  


Question 2: 
  
covid |>
  group_by(date) |>
  summarize(cases = sum(cases)) |>
  ggplot(aes(x = date, y = cases)) +
  geom_col(fill = "lightblue", color = "lightblue", alpha = .25) +
  geom_line(color = "navy", size = 3) +
  theme_grey() +
  labs(title = "Total USA COVID Cases per day",
       x = "Date",
       y = "Cases",
       caption = "Daily Exercise 07")
  

