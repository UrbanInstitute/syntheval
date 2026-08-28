library(tidymodels)
library(tidysynthesis)


# shared preprocessing ------------------------------------------------------

set.seed(20240726)

# load ACS confidential data
acs_conf <- syntheval::acs_conf

conf_data <- acs_conf %>%
  dplyr::select(
    county, gq, sex, marst, hcovany, empstat, classwkr, age, inctot
  )

conf_props <- conf_data %>% 
  dplyr::group_by(county, gq, sex, marst, hcovany, empstat, classwkr, 
                  .drop = FALSE) %>%
  dplyr::tally() %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(prop = n / nrow(acs_conf))

# lower-risk synthesis ------------------------------------------------------

#'
#' Create one lower-disclosure-risk synthetic data sample 
#' 
#' @param synth_id Integer, ID to associate with synthetic data replicate
#' 
sample_lr_synth <- function(synth_id) {
  
  # lower-risk categorical synthesis: sample from regularized cell frequencies
  lr_synth <- conf_props %>%
    dplyr::mutate(
      lr_n = stats::rmultinom(
        n = 1, 
        size = nrow(conf_data), 
        # mixture of 95% confidential data and 5% uniform sample
        prob = conf_props$prop * 0.95 + 0.5 / nrow(conf_props)
      )[, 1]
    ) %>%
    tidyr::uncount(weights = lr_n) %>%
    dplyr::select(-c(n, prop))
  
  # use sampled categorical variables as start data
  roadmap <- roadmap(
    conf_data = conf_data,
    start_data = lr_synth
  ) |>
    add_sequence_manual(c(age, inctot))

  # use a standard rpart decision tree for each variable
  rpart_reg <- parsnip::decision_tree(mode = "regression")
  
  rpart_class <- parsnip::decision_tree(mode = "classification")
  
  synth_spec <- synth_spec(
    default_regression_model = rpart_reg,
    default_classification_model = rpart_class,
    default_regression_sampler = sample_rpart,
    default_classification_sampler = sample_rpart
  )
  
  presynth <- presynth(
    roadmap = roadmap,
    synth_spec = synth_spec
  )
  

  return(
    
    # synthesize using tidysynthesis 
    synthesize(presynth)$synthetic_data %>%
      collapse_na() %>%
      dplyr::mutate(
        synth_id = synth_id,
        # add two-sided geometric row-wise noise to each numeric synthesis
        age = age + rgeom(n = nrow(acs_conf), prob = 0.5) - 
          rgeom(n = nrow(acs_conf), prob = 0.5),
        inctot = dplyr::if_else(
          inctot > 0, 
          round(inctot, -1) + 10 * (
            rgeom(n = nrow(acs_conf), prob = 0.2) - 
              rgeom(n = nrow(acs_conf), prob = 0.2)
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
#' Create one higher-disclosure-risk synthetic data sample 
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
        sample(1:nrow(acs_conf)) > round(0.05 * nrow(acs_conf))
      )
    ) %>%
    dplyr::filter(keep_ix == TRUE) %>%
    select(-keep_ix)
  
  # use sampled categorical variables as start data
  # synthesize two numeric variables, "age" and "inctot"
  roadmap <- roadmap(
    conf_data = conf_data,
    start_data = hr_cats
  ) |>
    add_sequence_manual(c(age, inctot))
  
  # define an intentionally overfit decision tree model
  overfit_rpart_reg <- parsnip::decision_tree(
    mode = "regression",
    tree_depth = 30,  # large max tree depth
    min_n = 2  # small terminal node size
  ) %>% 
    parsnip::set_engine(
      "rpart", xval = 0  # disable cross-validation for pruning
    )
  
  rpart_class <- parsnip::decision_tree(mode = "classification") 
  
  synth_spec <- synth_spec(
    default_regression_model = overfit_rpart_reg,
    default_classification_model = rpart_class,
    default_regression_sampler = sample_rpart,
    default_classification_sampler = sample_rpart
  )
  
  presynth <- presynth(
    roadmap = roadmap,
    synth_spec = synth_spec
  )
  
  return(
    # return synthesis result without modification
    synthesize(presynth)$synthetic_data %>%
      collapse_na() %>%
      dplyr::mutate(synth_id = synth_id)
  )

}

# synthesize and write to package
acs_hr_synths <- purrr::map(
  .x = 1:30, 
  .f = ~ sample_hr_synth(.x)
) 
usethis::use_data(acs_hr_synths, overwrite = TRUE)

