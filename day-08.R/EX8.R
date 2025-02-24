## Genesis Lacy 
## Due: February 24, 2025
## ESS 330 Daily Exercise 8

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
  
covid <- read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')
  
  df <- data.frame(
    region = state.region,
    abbr = state.abb,
    state = state.name)

head(df)
  
  covid_joined <- covid %>%
    inner_join(df, by = "state") %>%
    group_by(region, date) %>%
    summarize(
      cases = sum(cases, na.rm = TRUE),
      deaths = sum(deaths, na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = c(cases, deaths),
      names_to = "type",
      values_to = "count")
  
  ggplot(covid_joined, aes(x = date, y = count, color = type, group = region)) +
    geom_line(color = "navy", linewidth = 1.5) +
    facet_grid(type ~ region, scales = "free_y") +
    labs(
      title = "Total COVID Cases and Deaths by Region",
      x = "Date",
      y = "Count",
      caption = "ESS 330 Daily Exercise 8") +
    theme_minimal()
