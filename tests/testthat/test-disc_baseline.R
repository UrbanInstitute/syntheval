
test_that("disc_baseline error checking", {
  
  expect_error(
    disc_baseline("not a dataframe")
  )
  
  # fail on data.frames with no factor columns for QIDs
  expect_error( 
    disc_baseline(acs_conf[, c("inctot", "age")])
  )
  
  # fail on data.frames with non-factor provided QIDs
  expect_error(
    disc_baseline(acs_conf, qid_keys = c("inctot", "age"))
  )
  
  # fail on overlapping user-specified keys 
  expect_error(
    disc_baseline(acs_conf, qid_keys = c("hcovany"), sens_keys = c("hcovany"))
  )
  
})

test_that("disc_baseline without sensitivity metrics", {
  
  qid_only <- disc_baseline(
    eval_data = acs_conf, 
    qid_keys = c("county", "gq"), 
    sens_keys = FALSE
  )
  
  expect_equal(nrow(qid_only$qid_metrics), 24)
  
  expect_identical(
    names(qid_only$qid_metrics), 
    c("key_id", "county", "gq", "metric", "value")
  )
  
  expect_null(qid_only$sens_keys)
  expect_null(qid_only$sens_metrics)
  
})

test_that("disc_baseline basic functionality", {
  
  metrics <- disc_baseline(
    eval_data = acs_conf[complete.cases(acs_conf), ],
    qid_keys = c("county", "gq"), 
    sens_keys = c("hcovany", "inctot")
  )
  
  expect_equal(
    names(metrics$sens_metrics),
    c("key_id", "county", "gq", "raw_n", "prop", "metric", "sens_var", "value")
  )
  
  expect_true(
    all(metrics$metric %in% c("l_div", "t_close"))
  )
  
  # check all distinct l-diversities are integer-valued
  l_divs <- metrics$sens_metrics %>%
    dplyr::filter(.data[["metric"]] == "l_div") %>%
    dplyr::pull("value")
  expect_true(all(l_divs == as.integer(l_divs)))
  
  # check all KS t-closeness values are between 0 and 1
  ks_values <- metrics$sens_metrics %>%
    dplyr::filter(
      .data[["sens_var"]] == "inctot" &
        .data[["metric"]] == "t_close"
    ) %>%
    dplyr::pull("value")
  expect_true(all(ks_values >= 0 & ks_values <= 1))
  
})

test_that("disc_baseline na.rm functionality", {
  
  expect_warning(
    metrics <- disc_baseline(
      eval_data = acs_conf,
      qid_keys = c("county", "gq"), 
      sens_keys = c("hcovany", "inctot")
    )
  )

  expect_no_warning(
    metrics_narm <- disc_baseline(
      eval_data = acs_conf,
      qid_keys = c("county", "gq"), 
      sens_keys = c("hcovany", "inctot"),
      na.rm = TRUE
    )
  )
  
  expect_false(
    identical(metrics$sens_metrics, metrics_narm$sens_metrics)
  )
  
})
