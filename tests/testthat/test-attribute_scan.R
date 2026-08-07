
# shared toy fixtures ---------------------------------------

toy_conf <- data.frame(
  q = factor(c("A", "A", "A", "B", "B"), levels = c("A", "B")),
  t = factor(c("X", "X", "X", "Y", "X"), levels = c("X", "Y"))
)

toy_synth <- data.frame(
  q = factor(c("A", "A", "B", "B", "B"), levels = c("A", "B")),
  t = factor(c("X", "Y", "X", "X", "Y"), levels = c("X", "Y"))
)

toy_ed <- eval_data(conf_data = toy_conf, synth_data = toy_synth)

# attribute_scan input errors ---------------------------------------

test_that("attribute_scan input errors", {
  
  expect_error(
    attribute_scan("not an eval_data object", qid_keys = "q")
  )
  
  # fail on non-factor qid_keys
  expect_error(
    attribute_scan(
      eval_data(conf_data = acs_conf, synth_data = acs_lr_synths[[1]]),
      qid_keys = "age"
    )
  )
  
  # fail on non-factor target_keys
  expect_error(
    attribute_scan(
      eval_data(conf_data = acs_conf, synth_data = acs_lr_synths[[1]]),
      qid_keys = "county",
      target_keys = "age"
    )
  )
  
  # fail on overlapping qid_keys and target_keys
  expect_error(
    attribute_scan(
      eval_data(conf_data = acs_conf, synth_data = acs_lr_synths[[1]]),
      qid_keys = "county",
      target_keys = "county"
    )
  )
  
  # fail on qid_keys with mismatched factor levels between conf and synth data
  mismatched_synth <- toy_synth
  mismatched_synth$q <- factor(as.character(mismatched_synth$q), levels = c("A", "B", "C"))
  
  expect_error(
    attribute_scan(
      eval_data(conf_data = toy_conf, synth_data = mismatched_synth),
      qid_keys = "q",
      target_keys = "t"
    )
  )
  
})

# attribute_scan basic functionality ---------------------------------------

test_that("attribute_scan default target_keys", {
  
  scan <- attribute_scan(
    eval_data(conf_data = acs_conf, synth_data = acs_lr_synths[[1]]),
    qid_keys = c("county", "gq")
  )
  
  expect_identical(scan$qid_keys, c("county", "gq"))
  
  expect_identical(
    scan$target_keys,
    setdiff(
      c("county", "gq", "sex", "marst", "hcovany", "empstat", "classwkr"),
      c("county", "gq")
    )
  )
  
})

test_that("attribute_scan object structure", {
  
  scan <- attribute_scan(toy_ed, qid_keys = "q", target_keys = "t")
  
  expect_s3_class(scan, "attribute_scan")
  expect_true(is_attribute_scan(scan))
  
  expect_identical(
    names(scan),
    c("qid_keys", "target_keys", "synthetic", "confidential", "call")
  )
  
  for (source in c("confidential", "synthetic")) {
    
    expect_identical(
      names(scan[[source]]),
      c("equivalence_classes", "distributions")
    )
    
    expect_identical(
      names(scan[[source]]$equivalence_classes),
      c("key_id", "q", "raw_n", "prop")
    )
    
    expect_identical(
      names(scan[[source]]$distributions),
      c("key_id", "q", "target_var", "target_level", "n", "prob")
    )
    
    # probabilities sum to 1 within each equivalence class / target variable
    prob_sums <- scan[[source]]$distributions |>
      dplyr::group_by(key_id, target_var) |>
      dplyr::summarise(total = sum(prob), .groups = "drop") |>
      dplyr::pull(total)
    
    expect_equal(prob_sums, rep(1, length(prob_sums)))
    
  }
  
  # equivalence class row counts match a direct .aggregate_qid call
  expect_identical(
    scan$confidential$equivalence_classes,
    .aggregate_qid(toy_conf, keys = "q")
  )
  
  expect_identical(
    scan$synthetic$equivalence_classes,
    .aggregate_qid(toy_synth, keys = "q")
  )
  
})

test_that("attribute_scan conditional distribution values", {
  
  scan <- attribute_scan(toy_ed, qid_keys = "q", target_keys = "t")
  
  conf_probs <- scan$confidential$distributions |>
    dplyr::arrange(q, target_level) |>
    dplyr::pull(prob)
  
  # class A: all X (prob 1, 0); class B: 1 X, 1 Y (prob 0.5, 0.5)
  expect_equal(conf_probs, c(1, 0, 0.5, 0.5))
  
})
# regression test: real NA values in qid_keys ---------------------------------------

test_that("attribute_scan handles actual NA values in qid_keys without erroring", {
  
  # an actual NA (not a declared factor level) in a qid_keys column previously 
  # caused an error in .target_distribution()'s prob calculation, since the 
  # inline grouped sum()/if_else() combination could hit ambiguous vector 
  # recycling. This is a regression test confirming attribute_scan() runs 
  # without error and produces the expected completed grid for the NA class.
  na_conf <- data.frame(
    q = factor(c(NA, "A", "A", "B", "B"), levels = c("A", "B")),
    t = factor(c("X", "X", "X", "Y", "X"), levels = c("X", "Y"))
  )
  
  na_synth <- data.frame(
    q = factor(c(NA, "A", "A", "B", "B"), levels = c("A", "B")),
    t = factor(c("X", "X", "Y", "X", "Y"), levels = c("X", "Y"))
  )
  
  scan <- attribute_scan(
    eval_data(conf_data = na_conf, synth_data = na_synth),
    qid_keys = c("q"),
    target_keys = c("t")
  )
  
  na_dist <- scan$confidential$distributions %>%
    dplyr::filter(is.na(q)) %>%
    dplyr::arrange(target_level)
  
  # the q = NA class is completed against every target level, just like any 
  # other equivalence class: "X" was actually observed (n = 1, prob = 1), 
  # "Y" was not (n = 0, prob = 0) -- it is not dropped or NaN.
  expect_equal(nrow(na_dist), 2)
  expect_equal(na_dist$target_level, c("X", "Y"))
  expect_equal(na_dist$n, c(1, 0))
  expect_equal(na_dist$prob, c(1, 0))
  
})
