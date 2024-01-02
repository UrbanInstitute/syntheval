library(palmerpenguins)
library(tidyverse)
library(tidysynthesis)

penguins_complete <- penguins %>%
  select(-year) %>%
  filter(complete.cases(.)) %>%
  mutate(
    flipper_length_mm = as.numeric(flipper_length_mm),
    body_mass_g = as.numeric(body_mass_g)
  )

set.seed(1)

starting_data <- penguins_complete %>% 
  select(species, island) %>%
  slice_sample(n = nrow(penguins_complete), replace = TRUE)

# create a synthesis order based on correlation with bill_length_mm
visit_sequence <- visit_sequence(
  conf_data = c("sex", "bill_length_mm", "bill_depth_mm", "flipper_length_mm", 
                "body_mass_g"),
  start_data = starting_data,
  type = "manual"
)

# create an object that is the basis for all subsequent operations
roadmap <- roadmap(
  conf_data = penguins_complete,
  start_data = starting_data,
  visit_sequence = visit_sequence
)

# use library(parsnip) and library(recipes) to create specifications for
# each variable
rpart_mod <- parsnip::decision_tree() %>% 
  parsnip::set_engine(engine = "rpart") %>%
  parsnip::set_mode(mode = "regression")

rpart_mod_class <- parsnip::decision_tree() %>% 
  parsnip::set_engine(engine = "rpart") %>%
  parsnip::set_mode(mode = "classification")

algos <- construct_algos(
  roadmap = roadmap,
  default_algo = rpart_mod,
  custom_algos = list(
    list(
      vars = "sex", 
      algorithm = rpart_mod_class
    )
  )
)


synth_spec <- synth_spec(
  roadmap = roadmap,
  synth_algorithms = algos,
  recipes = construct_recipes(roadmap = roadmap),
  predict_methods = sample_rpart
)

presynth1 <- presynth(
  roadmap = roadmap,
  synth_spec = synth_spec
)

set.seed(1)
penguins_postsynth <- synthesize(presynth1, progress = TRUE)

usethis::use_data(penguins_postsynth, overwrite = TRUE)

penguins_syn_df <- penguins_postsynth$synthetic_data

usethis::use_data(penguins_syn_df, overwrite = TRUE)
