library(tidymodels)
library(palmerpenguins)
library(ranger)

set.seed(123)

(penguins_split <- initial_split(drop_na(penguins), prop = 0.7, strata = species))
#> <Training/Testing/Total>
#> <265/68/333>
penguins_train  <- training(penguins_split)
penguins_test   <- testing(penguins_split)

penguin_folds <- vfold_cv(penguins_train, v = 10)

print(penguin_folds)

# defining multinominal logistic regression model, there are 3 species types

multinom_model <- multinom_reg() %>%
  set_engine("nnet") %>%
  set_mode("classification")

# using default model which is nnet
rand_forest_model <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")

penguins_wf_Set <- workflow_set(
  preproc = list(species ~ .),
  models = list(multinom = multinom_model, rf = rand_forest_model))

# fit both models with the 10 fold cross validation, #fit_resamples is a function from tidymodels ecosystem, specicially in the workflows

penguins_res <- penguins_wf_Set %>%
  workflow_map("fit_resamples",
               resamples = penguin_folds, control = control_resamples(save_pred = TRUE))

# saving metrics
penguins_res_metrics <- collect_metrics(penguins_res)


# compare
accuracy_comparison <- penguins_res_metrics %>%
  filter(.metric == "accuracy")

print(accuracy_comparison)