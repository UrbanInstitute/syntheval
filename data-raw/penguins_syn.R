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
  select(species, island, sex) %>%
  slice_sample(n = nrow(penguins_complete), replace = TRUE)

# create a synthesis order based on correlation with bill_length_mm
visit_sequence <- visit_sequence(
  conf_data = penguins_complete,
  start_data = starting_data,
  type = "correlation",
  cor_var = "bill_length_mm"
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
  parsnip::set_engine("rpart")

synth_spec <- synth_spec(
  roadmap = roadmap,
  synth_algorithms = rpart_mod,
  recipes = construct_recipes(roadmap = roadmap),
  predict_methods = sample_rpart
)

# don't add extra noise to predictions
noise <- noise(
  roadmap = roadmap,
  add_noise = FALSE,
  exclusions = 0
)

# don't impose constraints
constraints <- constraints(
  roadmap = roadmap,
  constraints = NULL,
  max_z = 0
)

# only generate one synthetic data set
replicates <- replicates(
  replicates = 1,
  workers = 1,
  summary_function = NULL
)

presynth1 <- presynth(
  roadmap = roadmap,
  synth_spec = synth_spec,
  noise = noise, 
  constraints = constraints,
  replicates = replicates
)

set.seed(1)
penguins_syn <- synthesize(presynth1)

usethis::use_data(penguins_syn, overwrite = TRUE)
