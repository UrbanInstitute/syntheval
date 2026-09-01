# ---- fixtures ---------------------------------------------------------------
#
# conf v = 1, 2, 3, 4; synth v = 1, 1, 4, 4; a is categorical in both
# bins = 2 (width): cut at 2.5, so low = {1, 2}, high = {3, 4}

conf <- tibble::tibble(v = c(1, 2, 3, 4), a = c("x", "y", "x", "y"))
synth <- tibble::tibble(v = c(1, 1, 4, 4), a = c("x", "x", "y", "y"))

discretize <- function(synth_data = synth, conf_data = conf, ...) {
  .discretize_k_marginal_vars(
    synth_data = synth_data,
    conf_data = conf_data,
    vars = c("v", "a"),
    ...
  )
}

# ---- discretization ---------------------------------------------------------

test_that("only numeric variables are discretized", {
  result <- discretize(bins = 2, discretize_method = "width")

  expect_s3_class(result$conf_data$v, "factor")
  expect_s3_class(result$synth_data$v, "factor")
  expect_type(result$conf_data$a, "character")

  # breaks derive from the confidential data and apply to both datasets
  expect_equal(levels(result$conf_data$v), levels(result$synth_data$v))
})

test_that("width bins split the confidential range evenly", {
  result <- discretize(bins = 2, discretize_method = "width")

  # conf 1:4 -> two bins of two; synth 1, 1, 4, 4 -> two bins of two
  expect_equal(as.vector(table(result$conf_data$v)), c(2, 2))
  expect_equal(as.vector(table(result$synth_data$v)), c(2, 2))
})

test_that("outer bins extend to +/-Inf", {
  synth_wide <- dplyr::mutate(synth, v = c(-100, -100, 100, 100))

  result <- discretize(
    synth_data = synth_wide,
    bins = 2,
    discretize_method = "width"
  )

  # nothing falls outside the bins
  expect_false(anyNA(result$synth_data$v))
  expect_equal(as.vector(table(result$synth_data$v)), c(2, 2))
})

test_that("missing numeric values stay missing after discretization", {
  # NA is left for downstream na.rm handling rather than forced into a bin
  synth_na <- dplyr::mutate(synth, v = c(1, 4, NA, NA))

  result <- discretize(
    synth_data = synth_na,
    bins = 2,
    discretize_method = "width"
  )

  expect_equal(is.na(result$synth_data$v), c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(as.vector(table(result$synth_data$v)), c(1, 1))
})

# ---- degenerate confidential values -----------------------------------------

test_that("too few distinct confidential values reduce bins with a warning", {
  conf_const <- dplyr::mutate(conf, v = 2)

  for (method in c("width", "ntile", "cluster")) {
    expect_warning(
      result <- discretize(
        conf_data = conf_const,
        bins = 2,
        discretize_method = method
      ),
      regexp = "has only 1 distinct confidential values"
    )

    expect_length(levels(result$conf_data$v), 1)
  }
})

test_that("tied quantile cut points collapse bins with a warning", {
  # passes the distinct-value check, but the 25th and 50th percentiles
  # coincide at 1, collapsing a quantile bin
  conf_ties <- tibble::tibble(v = c(1, 1, 1, 1, 1, 1, 2, 3, 4, 5), a = "x")
  synth_5 <- tibble::tibble(v = 1:5, a = "x")

  expect_warning(
    result <- discretize(
      synth_data = synth_5,
      conf_data = conf_ties,
      bins = 4,
      discretize_method = "ntile"
    ),
    regexp = "bins instead of 4 because of tied cut points"
  )

  expect_lt(length(levels(result$conf_data$v)), 4)
})

# ---- input errors -----------------------------------------------------------

test_that(".discretize_k_marginal_vars() input errors work", {
  # bins must be a single integer >= 2
  expect_error(
    discretize(bins = 1, discretize_method = "width"),
    regexp = "`bins` must be a single integer >= 2"
  )

  # confidential values must be finite
  conf_inf <- dplyr::mutate(conf, v = c(1, 2, 3, Inf))

  expect_error(
    discretize(conf_data = conf_inf, bins = 2, discretize_method = "width"),
    regexp = "must be finite to discretize"
  )
})
