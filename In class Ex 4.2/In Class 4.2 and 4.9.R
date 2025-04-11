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
#something about how it further narrows down which model to use
mutate(logC = log(season_cases +1))

skimr::skim(state_data)


# ML applications

set.seed(123)
split <- inital_split(state_data, prop =.8, strata = season) # 80/20 split on the data
training <-training(split)
testsing <- testing(split)

folds <- vfolds_cv(train, v = 10)


# general note: do NOT touch your outcome variable when making a recipe
rec <- recipe(season_cases ~.,date = training) |>
  step_rm(state, season_cases) |>
  step_dummy(all_nominal_predictors()) |>
  step_scale(all_nominal_predictors()) |>
  step_center(all_numeric_predictors())


lm_mod <-linear_reg() |>
  set_engine('lm')|>
  set_mode("regression")

rf_mod <-rand_forest() |>
  set_engine('ranger')|>
  set_mode("regression")

rf_mod2 <-rand_forest() |>
  set_engine('randomForest')|>
  set_mode("regression")

b_mod <-boost_tree() |>
  set_engine('xgboost')|>
  set_mode("regression")

nn_mod <-mlp(hidden = 10) |>
  set_engine('nnet')|>
  set_mode("regression")


wf = workflow_set(list(rec), list(lm_mod,
                                   rf_mod,
                                   rf_mod2,
                                   b_mod,
                                   nn_mod))|>
  workflow_map(resample = folds)

## can tell us what model works best - can compare with rsq values
autoplot(wf)
  
b_fit = workflow() |>
  add_recipe(rec)|>
  add_model(b_mod) |>
  fit(data = training)


a = augment (b_fit, new_data = training)

ggplot(a, aes( x = .pred, y = logC)) +
  
  geom_point()


vip::vip(b_fit)
               
  


## --- DAY 2 DEMO ---

?boost_tree


b_model_tune <- boost_tree(trees = tune(), tree_depth = tune(), min_n = tune()) |> 
  set_engine("lightgbm") |> 
  set_mode("regression")


# Model specification

wf_tune <-  workflow(rec, b_model_tune) 


# we can ask for particular merics with this
covid_metrics = metric_set(rsq, rmse, mae)


dials <- extract_parameter_set_dials(wf_tune) 
dials$object

# choosing a combination of sets to test, creating a grid of options to see which one gives us the best results
my.grid <- dials |> 
  grid_latin_hypercube(size = 25)

range(my.grid$trees)

plotly::plot_ly(my.grid, 
                x = ~trees, 
                y = ~tree_depth, 
                z = ~min_n)

# need to add wf that has tunable states, training resampled data, the grid and calculating the metrics using the covid metrics
model_params <-  tune_grid(
  wf_tune,
  resamples = folds,
  grid = my.grid,
  metrics = covid_metrics
)

autoplot(model_params)

show_best(model_params, metric = "rsq")
show_best(model_params, metric = "rmse")
show_best(model_params, metric = "mae")

## In class code
hp = select_best(model_params, metric = "mae")
finalize_workflow(wf_tune, hp)

final_wf = finalize_workflow(wf_tune,hp)

last_f <- last_fit(final_wf, split)

collecct_metrics(last_f)

collect_prediction(last_f)
pred = collect_predictions(last_f)

ggplot(pred, aes(x = logC, y = .pred)) +
  geom_point() +
  ##draws a line for us and can add lm
  geom_smooth(method = "lm") +
  geom_abline(col = "red") +
  theme_linedraw()
  
final_fit <-fit(final_wf, data = state_data)




## code from github
hp_best <- select_best(model_params, metric = "mae")

finalize <- finalize_workflow(wf_tune, hp_best)

final_fit <- last_fit(finalize, split, metrics = covid_metrics)

collect_metrics(final_fit)

collect_predictions(final_fit) |> 
  ggplot(aes(x = .pred, y = logC)) + 
  geom_point() +
  geom_abline() + 
  geom_smooth(method = "lm") + 
  theme_linedraw() + 
  labs(title = "Final Fit", 
       x = "Predicted (Log10)", 
       y = "Actual (Log10)")

full_pred = fit(finalize, data = state_data) |>
  augment(new_data = state_data) 

ggplot(full_pred, aes(x = .pred, y = logC, color = as.factor(y))) + 
  geom_point() +
  geom_abline() + 
  stat_ellipse() +
  geom_smooth(method = "lm") + 
  theme_linedraw() + 
  labs(title = "Final Fit", 
       x = "Predicted (Log10)", 
       y = "Actual (Log10)")

### ---- Streamlined Version ---- ####
wf_tune <- workflow(rec, 
                    boost_tree(mode       = "regression", 
                               engine     = "lightgbm", 
                               trees      = tune(), 
                               tree_depth = tune()))

set.seed(1)
hp_vars <- tune_grid(wf_tune,
                     resamples = folds,
                     grid =  25, # grid_space_filling used by default,
                     metrics = covid_metrics) |> 
  select_best(metric = "mae")

finalize <- finalize_workflow(wf_tune, hp_vars) |> 
  last_fit(split, metrics = covid_metrics)

collect_metrics(finalize)

  