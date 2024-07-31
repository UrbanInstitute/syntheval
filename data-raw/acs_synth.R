library(tidymodels)
library(tidysynthesis)


# shared preprocessing ------------------------------------------------------

set.seed(20240726)

# load ACS confidential data
data(acs_conf)

conf_data <- acs_conf %>%
  dplyr::select(
    county, gq, sex, marst, hcovany, empstat, classwkr, age, inctot
  )

conf_props <- acs_conf %>% 
  dplyr::group_by(county, gq, sex, marst, hcovany, empstat, classwkr, 
                  .drop = FALSE) %>%
  dplyr::tally() %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(prop = n / dim(acs_conf)[1])

# lower-risk synthesis ------------------------------------------------------

#'
#' Create one lower-disclosure-risk sample 
#' 
#' @param synth_id Integer, ID to associate with synthetic data replicate
#' 
sample_lr_synth <- function(synth_id) {
  
  # lower-risk categorical synthesis: sample from regularized cell frequencies
  lr_synth <- conf_props %>%
    dplyr::mutate(
      lr_n = rmultinom(
        1, 
        dim(conf_data)[1], 
        # mixture of 95% confidential data and 5% uniform sample
        conf_props$prop * .95 + .5 / dim(conf_props)[1]
      )[, 1]
    ) %>%
    tidyr::uncount(weights = lr_n) %>%
    dplyr::select(-c(n, prop))
  
  # use sampled categorical variables as start data
  schema <- schema(
    conf_data = conf_data,
    start_data = lr_synth
  )
  
  # synthesize two numeric variables, "age" and "inctot"
  visit_sequence <- visit_sequence(
    schema = schema,
    type = "manual",
    manual_vars = c("age", "inctot")
  )
  
  roadmap <- roadmap(visit_sequence = visit_sequence)
  
  # use a standard rpart decision tree for each variable
  rpart_reg <- parsnip::decision_tree(mode = "regression")
  
  synth_spec <- synth_spec(
    roadmap = roadmap,
    synth_algorithms = rpart_reg,
    recipes = construct_recipes(roadmap = roadmap),
    predict_methods = sample_rpart
  )
  
  presynth <- presynth(
    roadmap = roadmap,
    synth_spec = synth_spec
  )
  

  return(
    
    # synthesize using tidysynthesis 
    synthesize(presynth)$synthetic_data %>%
      dplyr::mutate(
        synth_id = synth_id,
        # add two-sided geometric row-wise noise to each numeric synthesis
        age = age + rgeom(dim(acs_conf)[1], .5) - rgeom(dim(acs_conf)[1], .5),
        inctot = dplyr::if_else(
          inctot > 0, 
          round(inctot, -1) + 10 * (
            rgeom(dim(acs_conf)[1], .2) - rgeom(dim(acs_conf)[1], .2)
          ),
          inctot
        )
      )
    
  )
  
}

# synthesize and write to package
acs_lr_synths <- purrr::map(
  .x = 1:30, 
  .f = ~ sample_lr_synth(.x)
) 
usethis::use_data(acs_lr_synths, overwrite = TRUE)


# higher-risk synthesis ------------------------------------------------------
  
#'
#' Create one higher-disclosure-risk sample 
#' 
#' @param synth_id Integer, ID to associate with synthetic data replicate
#' 
sample_hr_synth <- function(synth_id) {
  
  # starting categoricals: resample x% of data uniformly from the original dataset
  hr_cats <- acs_conf %>%
    dplyr::select(
      county, gq, sex, marst, hcovany, empstat, classwkr
    ) %>%
    dplyr::mutate(
      keep_ix = (
        sample(1:dim(acs_conf)[1]) > round(.05 * dim(acs_conf)[1])
      )
    ) %>%
    dplyr::filter(keep_ix == TRUE) 
  
  # use sampled categorical variables as start data
  schema <- schema(
    conf_data = conf_data,
    start_data = hr_synth
  )
  
  # synthesize two numeric variables, "age" and "inctot"
  visit_sequence <- visit_sequence(
    schema = schema,
    type = "manual",
    manual_vars = c("age", "inctot")
  )
  
  roadmap <- roadmap(visit_sequence = visit_sequence)
  
  # define an intentionally overfit decision tree model
  overfit_rpart_reg <- parsnip::decision_tree(
    mode = "regression",
    tree_depth = 30,  # large max tree depth
    min_n = 2  # small terminal node size
  ) %>% 
    parsnip::set_engine(
      "rpart", xval = 0  # disable cross-validation for pruning
    )
  
  synth_spec <- synth_spec(
    roadmap = roadmap,
    synth_algorithms = overfit_rpart_reg,
    recipes = construct_recipes(roadmap = roadmap),
    predict_methods = sample_rpart
  )
  
  presynth <- presynth(
    roadmap = roadmap,
    synth_spec = synth_spec
  )
  
  return(
    # return synthesis result without modification
    synthesize(presynth)$synthetic_data %>%
      dplyr::mutate(synth_id = synth_id)
  )

}

# synthesize and write to package
acs_hr_synths <- purrr::map(
  .x = 1:30, 
  .f = ~ sample_hr_synth(.x)
) 
usethis::use_data(acs_hr_synths, overwrite = TRUE)

