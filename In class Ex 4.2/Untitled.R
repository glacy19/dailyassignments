library(tidyverse)
library(tidymodels)

#ingest covid
covid_url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv' 

pop_url <-'https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/state/totals/NST-EST2024-ALLDATA.csv'
  
  
data <- readr::read_csv(covid_url)
census_raw <- readr::read_csv(pop_url)


census <- census_raw |>
  filter(COUNTY == "000") |>
  mutate(fips = STATE) |>
  select(fips, POPESTIMATE, DEATHS2021, BIRTHS2021)

state_data <- data |>
  group_by(fips)|>
  mutate(new_cases = case - lag(cases),
         new_deaths = deaths - lag(deaths))
ungroup() |>
  left_join(census, by = "fips") |>
  mutate(y = year(date), m = month(date),
         season = case_when(
           m %in% c(12,1,2) ~ "Winter",
           m %in% 3:5 ~ "Spring",
           m %in% 6:8 ~ "Summer",
           m %in% 9:11 ~ "Fall"
         )) |>
group_by(state, season) |>
mutate(season_cases = sum(new_cases, na.rm = TRUE),
       season_deaths = sum(new_deaths, na.rm = TRUE)) |>
distinct(state, y, season, .keep_all = TRUE)
ungroup() |>
select(state, contains("seasons"), contains("2021")) |>
  drop_na()

skimr::skim(state_data)


# ML applications

set.seed(123)
split <- inital_split(state_data, prop =.8, strata = season)

training <-training(split)
testsing <- testing(split)

folds <- vfolds_cv(train, v = 10)






