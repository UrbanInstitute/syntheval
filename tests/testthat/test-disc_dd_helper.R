
# .aggregate_qid tests ---------------------------------------

test_that(".aggregate_qid basic functionality", {
  
  res <- .aggregate_qid(
    df = acs_conf, 
    keys = c("county", "gq")
  )

  # check expected column names
  expect_identical(
    names(res), 
    c("key_id", "county", "gq", "raw_n", "prop")
  )
  
  # check expected column types
  expect_identical(
    purrr::map_chr(.x = res, .f = ~ pillar::type_sum(.x)),
    c("key_id" = "int", 
      "county" = "fct", 
      "gq" = "fct", 
      "raw_n" = "int", 
      "prop" = "dbl")
  )
  
  # check robustness to reordering
  set.seed(202407)
  res2 <- .aggregate_qid(
    df = acs_conf %>% 
      dplyr::slice_sample(n = dim(acs_conf)[1], replace = FALSE),
    keys = c("county", "gq")
    )
  
  expect_identical(res, res2)
  
  # check inclusion of missing keys
  res3 <- .aggregate_qid(
    df = acs_conf %>% 
      dplyr::filter(county == "Douglas" & gq == "Household"),
    keys = c("county", "gq")
  )
  
  expect_identical(
    res %>% 
      dplyr::select(key_id, county, gq),
    res3 %>% 
      dplyr::select(key_id, county, gq)
  )
  
})

# .validate_eval_keys tests ---------------------------------------

test_that(".validate_eval_keys validates valid eval_data objects", {
  
  expect_equal(
    .validate_eval_keys(
      eval_data(
        conf_data = acs_conf, 
        synth_data = acs_lr_synths
      ), 
      keys = c("county", "gq")
    ),
    0
  )
  
  expect_equal(
    .validate_eval_keys(
      eval_data(
        conf_data = acs_conf, 
        synth_data = acs_lr_synths[[1]]
      ), 
      keys = c("county", "gq")
    ),
    0
  )
  
  expect_equal(
    .validate_eval_keys(
      eval_data(
        conf_data = acs_conf, 
        synth_data = acs_lr_synths,
        holdout_data = acs_conf
      ), 
      keys = c("county", "gq")
    ),
    0
  )
  
  expect_equal(
    .validate_eval_keys(
      eval_data(
        conf_data = acs_conf %>%
          dplyr::select(county, gq), 
        synth_data = acs_lr_synths[[1]] %>%
          dplyr::select(county, gq)
      )
    ),
    0
  )
  
})


test_that(".validate_eval_keys fails when expected", {
  
  expect_error(
    .validate_eval_keys(
      eval_data(
        conf_data = acs_conf,
        synth_data = mtcars
      ),
      keys = c("county", "gq")
    )
  )
  
  expect_error(
    .validate_eval_keys(
      eval_data(
        conf_data = acs_conf,
        synth_data = acs_lr_synths,
        holdout_data = mtcars
      ),
      keys = c("county", "gq")
    )
  )
  
  expect_error(
    .validate_eval_keys(
      eval_data(
        conf_data = acs_conf,
        synth_data = append(acs_lr_synths, list(mtcars))
      ),
      keys = c("county", "gq")
    )
  )
  
  expect_error(
    .validate_eval_keys(
      eval_data(
        conf_data = acs_conf,
        synth_data = acs_lr_synths[[1]] %>%
          dplyr::mutate(
            county = forcats::fct_collapse(
              gq, 
              hi = c("Household", "Institution"))
          )
      ),
      keys = c("county", "gq")
    )
  )
  
})


# prep_discrete_eval_data tests ---------------------------------------


orig_ed <- eval_data(
  conf_data = acs_conf, 
  synth_data = acs_lr_synths,
  holdout_data = acs_conf
)

disc_ed1 <- prep_discrete_eval_data(
  orig_ed,
  col_map = list(
    "age" = list("k" = 10), 
    "inctot" = list("width" = 10000)
  )
)

test_that("prep_discrete_eval_data type functionality", {
  
  # check column types
  expect_identical(
    pillar::type_sum(disc_ed1$conf_data$age), "fct"
  )
  expect_identical(
    pillar::type_sum(disc_ed1$synth_data[[1]]$age), "fct"
  )
  expect_identical(
    pillar::type_sum(disc_ed1$holdout_data$age), "fct"
  )
  
  expect_identical(
    pillar::type_sum(disc_ed1$conf_data$inctot), "fct"
  )
  expect_identical(
    pillar::type_sum(disc_ed1$synth_data[[1]]$inctot), "fct"
  )
  expect_identical(
    pillar::type_sum(disc_ed1$holdout_data$inctot), "fct"
  )
  
  # check identical factor level mappings
  expect_identical(
    levels(disc_ed1$conf_data$age), 
    levels(disc_ed1$synth_data[[1]]$age)
  )
  expect_identical(
    levels(disc_ed1$conf_data$age), 
    levels(disc_ed1$holdout_data$age)
  )
  
  expect_identical(
    levels(disc_ed1$conf_data$inctot), 
    levels(disc_ed1$synth_data[[1]]$inctot)
  )
  expect_identical(
    levels(disc_ed1$conf_data$inctot), 
    levels(disc_ed1$holdout_data$inctot)
  )
  
  # check that first level is NA
  expect_identical(
    levels(disc_ed1$holdout_data$age)[1], 
    NA_character_
  )
  
  expect_identical(
    levels(disc_ed1$holdout_data$inctot)[1], 
    NA_character_
  )
  
}) 


test_that("prep_discrete_eval_data boundary construction", {
  
  # check discretization logic 
  expect_equal(length(levels(disc_ed1$conf_data$age)), 11)
  
  # check that first interval includes the smallest value
  first_boundary <- stringr::str_extract(
    levels(disc_ed1$conf_data$inctot), "(?<=\\[).*?(?=\\])"
  ) %>% 
    stringr::str_split_fixed(",", n = 2)
  
  expect_equal(
    as.numeric(first_boundary[2, 2]) - as.numeric(first_boundary[2, 1]), 
    10000
  )
  
  # extract boundaries for all except the first window (left-open intervals)
  boundaries <- stringr::str_extract(
    levels(disc_ed1$conf_data$inctot), "(?<=\\().*(?=\\])"
  ) %>%
    stringr::str_split_fixed(",", n = 2)
  
  # all except the first two rows (NA) and last boundary...
  boundaries <- boundaries[
    3:(length(levels(disc_ed1$conf_data$inctot)) - 1), 
  ] %>% 
    base::apply(2, as.numeric)
  
  # ...should be equal width
  expect_equal(
    unique(round(boundaries[2, ] - boundaries[1, ], -4)),
    10000
  )
  
})

test_that("prep_discrete_eval_data boundary application", {
  
  # ensure each value gets mapped to a non-trivial factor level
  expect_false(
    NA %in% unique(disc_ed1$conf_data$age)
  )
  expect_false(
    NA %in% unique(disc_ed1$synth_data$age)
  )
  expect_false(
    NA %in% unique(disc_ed1$holdout_data$age)
  )
  
})


