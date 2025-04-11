library(tidyverse)
library(tidymodels)

#ingest covid
covid_url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv' 

pop_url <-'https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/state/totals/NST-EST2024-ALLDATA.csv'


data <- readr::read_csv(covid_url)
census_data <- readr::read_csv('https://raw.githubusercontent.com/mikejohnson51/csu-ess-330/refs/heads/main/resources/co-est2023-alldata.csv')


census <- census_data |>
  filter(COUNTY == "000") |>
  mutate(fips = as.character(STATE)) |>
  select(fips, contains ("2021"))

state_data <- data |>
  group_by(state)|>
  mutate(new_cases = cases - lag(cases),
         new_deaths = deaths - lag(deaths))|>
  ungroup() |> #explicitly ungroup the grouped data
  left_join(census, by = "fips") |>
  mutate(y = year(date), m = month(date),
         season = case_when(
           m %in% c(12,1,2) ~ "Winter",
           m %in% 3:5 ~ "Spring",
           m %in% 6:8 ~ "Summer",
           m %in% 9:11 ~ "Fall"
         )) |>
  
  group_by(state, y, season) |> 
  mutate(
    season_cases  = sum(new_cases, na.rm = TRUE),  # Aggregate seasonal cases
    season_deaths = sum(new_deaths, na.rm = TRUE)  # Aggregate seasonal deaths
  )  |> 
  distinct(state, y, season, .keep_all = TRUE) |>  # Keep only distinct rows by state, year, season
  ungroup() |> 
  select(state, contains('season'), y, POPESTIMATE2021, BIRTHS2021, DEATHS2021) |>  # Select relevant columns
  drop_na(season, contains("2021")) |>  # Remove rows with missing values
  mutate(logC = log(season_cases +1))  # Log-transform case numbers for modeling

skimr::skim(state_data)


# ML applications;
set.seed(123)
split <- initial_split(state_data, prop =.8, strata = season) # sets 80/20 split on the data
train <-training(split)
test <- testing(split)
folds <- vfold_cv(train, v = 10)


# general note: do NOT touch your outcome variable when making a recipe
rec <- recipe(logC ~.,data = train) |>
  step_rm(state, season_cases) |>
  step_dummy(all_nominal()) |>
  step_scale(all_numeric_predictors()) |>
  step_center(all_numeric_predictors())


lm_model <-linear_reg() |>
  set_engine('lm')|>
  set_mode("regression")

rf_model <-rand_forest() |>
  set_engine('ranger')|>
  set_mode("regression")

rf_model2 <-rand_forest() |>
  set_engine('randomForest')|>
  set_mode("regression")

b_model <-boost_tree() |>
  set_engine('xgboost')|>
  set_mode("regression")

nn_model <-mlp(hidden = 10) |>
  set_engine('nnet')|>
  set_mode("regression")


wf = workflow_set(list(rec), list(lm_model,
                                  rf_model,
                                  rf_model2,
                                  b_model,
                                  nn_model))|>
  workflow_map(resamples = folds)

## can tell us what model works best - can compare with rsq values
autoplot(wf)

best_fit = workflow() |>
  add_recipe(rec)|>
  add_model(INSERT MODEL THAT IS BEST) |>
  fit(data = train)

predictions <- augment(fit, new_data = test) |>
  mutate(diff = abs(logC - .pred))

metrics (predictions, truth = logC, estimate = .pred)

ggplot(predictions, aes( x = logC, y = ,pred)) +
  geom_point() +
  geeom_abline() + 
  
  labs(title = "Neural Net Model",
       x = "Actual(Log10)",
       y = "Predicted (Log10)")+ 
  theme_minimal
