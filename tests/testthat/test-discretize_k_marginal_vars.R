test_that("only numeric variables are discretized", {
  conf_mix <- tibble::tibble(v = c(1, 2, 3, 4), a = c("x", "y", "x", "y"))
  synth_mix <- tibble::tibble(v = c(1, 1, 4, 4), a = c("x", "x", "y", "y"))

  result <- .discretize_k_marginal_vars(
    synth_data = synth_mix,
    conf_data = conf_mix,
    vars = c("v", "a"),
    bins = 2,
    discretize_method = "width"
  )

  expect_s3_class(result$conf_data$v, "factor")
  expect_s3_class(result$synth_data$v, "factor")
  expect_type(result$conf_data$a, "character")

  # breaks derive from the confidential data and apply to both datasets
  expect_equal(levels(result$conf_data$v), levels(result$synth_data$v))
})

test_that("invalid bins values error", {
  conf_num <- tibble::tibble(v = c(1, 2, 3, 4))

  for (bad in list(1, 2.5, "2", NA_real_, c(2, 3))) {
    expect_error(
      .discretize_k_marginal_vars(
        synth_data = conf_num,
        conf_data = conf_num,
        vars = "v",
        bins = bad,
        discretize_method = "width"
      ),
      regexp = "`bins` must be a single integer >= 2"
    )
  }
})

test_that("too few distinct confidential values throw an error", {
  conf_const <- tibble::tibble(v = c(2, 2, 2, 2))
  synth_num <- tibble::tibble(v = c(1, 1, 1, 4))

  for (method in c("width", "ntile", "cluster")) {
    expect_error(
      .discretize_k_marginal_vars(
        synth_data = synth_num,
        conf_data = conf_const,
        vars = "v",
        bins = 2,
        discretize_method = method
      ),
      regexp = "fewer distinct confidential values than `bins`"
    )
  }
})

test_that("tied quantile cut points collapse bins with a warning", {
  # heavily tied data passes the distinct-value pre-check, but the 25th and
  # 50th percentiles coincide at 1, collapsing a quantile bin
  conf_ties <- tibble::tibble(v = c(1, 1, 1, 1, 1, 1, 2, 3, 4, 5))
  synth_num <- tibble::tibble(v = c(1, 2, 3, 4, 5))

  expect_warning(
    result <- .discretize_k_marginal_vars(
      synth_data = synth_num,
      conf_data = conf_ties,
      vars = "v",
      bins = 4,
      discretize_method = "ntile"
    ),
    regexp = "bins instead of 4 because of tied cut points"
  )

  expect_lt(length(levels(result$conf_data$v)), 4)
})

test_that("infinite confidential values refuse to discretize", {
  conf_inf <- tibble::tibble(v = c(1, 2, 3, Inf))
  synth_num <- tibble::tibble(v = c(1, 2, 3, 3))

  expect_error(
    .discretize_k_marginal_vars(
      synth_data = synth_num,
      conf_data = conf_inf,
      vars = "v",
      bins = 2,
      discretize_method = "width"
    ),
    regexp = "must be finite to discretize"
  )
})
