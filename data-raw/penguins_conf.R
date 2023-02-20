library(palmerpenguins)
library(tidyverse)

penguins_conf <- penguins %>%
  select(-year) %>%
  filter(complete.cases(.)) %>%
  mutate(
    flipper_length_mm = as.numeric(flipper_length_mm),
    body_mass_g = as.numeric(body_mass_g)
  )

usethis::use_data(penguins_conf, overwrite = TRUE)
