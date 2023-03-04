
# import data analysis packages
library(tidyverse)

# import palmer penguins data
penguins <- read_csv("https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/inst/extdata/penguins.csv")

# preprocess data and remove missing values
penguins <- penguins %>%
  drop_na()

# preprocess features for exploratory data analysis
# convert character variables to factors
penguins <- penguins  %>% 
  mutate_if(is.character, as.factor)

# create a new feature for penguin species

penguins <- penguins %>%
  mutate(species = case_when(
    species == "Adelie" ~ "Adelie",
    species == "Chinstrap" ~ "Chinstrap",
    species == "Gentoo" ~ "Gentoo"
  ))

# plot penguin species
penguins %>%
  ggplot(aes(x = species)) +
  geom_bar() +
  labs(title = "Penguin Species",
       x = "Species",
       y = "Count")


# build machine leanring model for penguin sex prediction

# import machine learning packages
library(tidymodels)
library(rsample)
library(recipes)
library(workflows)
library(yardstick)
library(parsnip)

penuins_split <- initial_split(penguins, prop = 0.8, strata = species)
penguins_train <- training(penuins_split)
penguins_test <- testing(penuins_split)

# preprocess features for machine learning model

base_recipe <- recipe(sex ~ ., data = penguins_train) %>%
  step_dummy(all_nominal(), -sex) %>%
  step_normalize(all_numeric())

# build machine learning model

rf_spec <- rand_forest(mode = "classification") %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_wflow <- workflow() %>%
  add_recipe(base_recipe) %>%
  add_model(rf_spec)

# train machine learning model

rf_fit <- rf_wflow %>%
  fit(data = penguins_train)

# evaluate machine learning model

rf_fit %>%
  predict(penguins_test) %>%
  bind_cols(penguins_test) %>%
  metrics(truth = sex, estimate = .pred_class)


# save machine learning model

saveRDS(rf_fit, "rf_fit.rds")

# deploy machine learning model

# ???
















