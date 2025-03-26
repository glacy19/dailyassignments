library(tidymodels)
library(palmerpenguins)

set.seed(123)

(penguins_split <- initial_split(drop_na(penguins), prop = 0.7, strata = species))
#> <Training/Testing/Total>
#> <265/68/333>
penguins_train  <- training(penguins_split)
penguins_test   <- testing(penguins_split)

penguin_folds <- vfold_cv(penguins_train, v = 10)

penguins_folds

