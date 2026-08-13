# hand-computable example
#
# conf_data          synth_data
#   a  b               a  b
#   x  p               x  p
#   x  p               y  q
#   y  p               y  q
#   y  q               y  p
#
# k = 1
#   marginal a: conf (x = 0.50, y = 0.50), synth (x = 0.25, y = 0.75)
#     MabsDD = mean(|0.25 - 0.50|, |0.75 - 0.50|) = 0.25
#   marginal b: conf (p = 0.75, q = 0.25), synth (p = 0.50, q = 0.50)
#     MabsDD = mean(|0.50 - 0.75|, |0.50 - 0.25|) = 0.25
#   score = (1 - mean(0.25, 0.25)) * 1000 = 750
#
# k = 2 (single combination: a x b)
#   cells: conf (x,p = 0.50, y,p = 0.25, y,q = 0.25)
#          synth (x,p = 0.25, y,p = 0.25, y,q = 0.50)
#     per-cell |synth - conf|: (x,p) 0.25, (y,p) 0, (y,q) 0.25
#     MabsDD = mean(0.25, 0, 0.25) = 1/6
#   score = (1 - 1/6) * 1000 = 5000/6

conf <- tibble::tibble(
  a = c("x", "x", "y", "y"),
  b = c("p", "p", "p", "q")
)

synth <- tibble::tibble(
  a = c("x", "y", "y", "y"),
  b = c("p", "q", "q", "p")
)

test_that("k = 1 score matches hand-computed value", {

  expect_equal(
    .util_k_marginals(synth_data = synth, conf_data = conf, k = 1),
    750
  )

})

test_that("k = 2 score matches hand-computed value", {

  expect_equal(
    .util_k_marginals(synth_data = synth, conf_data = conf, k = 2),
    5000 / 6
  )

})

test_that("identical data scores exactly 1000 for every k", {

  for (k in 1:2) {

    expect_equal(
      .util_k_marginals(synth_data = conf, conf_data = conf, k = k),
      1000
    )

  }

})

test_that("cells absent from one dataset count as proportion zero", {

  # conf has level y that synth lacks; synth is all x
  # marginal a: conf (x = 0.5, y = 0.5), synth (x = 1, y = 0)
  # MabsDD = mean(0.5, 0.5) = 0.5 -> score 500
  conf_gap <- tibble::tibble(a = c("x", "y"))
  synth_gap <- tibble::tibble(a = c("x", "x"))

  expect_equal(
    .util_k_marginals(synth_data = synth_gap, conf_data = conf_gap, k = 1),
    500
  )

})

test_that("k outside 1:3 throws an error", {

  expect_error(
    .util_k_marginals(synth_data = synth, conf_data = conf, k = 4),
    regexp = "`k` must be a single integer between 1 and 3"
  )

  expect_error(
    .util_k_marginals(synth_data = synth, conf_data = conf, k = 0),
    regexp = "`k` must be a single integer between 1 and 3"
  )

  expect_error(
    .util_k_marginals(synth_data = synth, conf_data = conf, k = c(1, 2)),
    regexp = "`k` must be a single integer between 1 and 3"
  )

  # %in% coerces, so non-numeric scalars need an explicit type check
  expect_error(
    .util_k_marginals(synth_data = synth, conf_data = conf, k = TRUE),
    regexp = "`k` must be a single integer between 1 and 3"
  )

  expect_error(
    .util_k_marginals(synth_data = synth, conf_data = conf, k = "1"),
    regexp = "`k` must be a single integer between 1 and 3"
  )

})

test_that("zero-row inputs throw an error instead of returning NaN", {

  empty <- conf[0, ]

  expect_error(
    .util_k_marginals(synth_data = empty, conf_data = conf, k = 1),
    regexp = "at least one row"
  )

  expect_error(
    .util_k_marginals(synth_data = synth, conf_data = empty, k = 1),
    regexp = "at least one row"
  )

})

test_that("k exceeding the number of shared variables throws an error", {

  expect_error(
    .util_k_marginals(synth_data = synth, conf_data = conf, k = 3),
    regexp = "shared by both datasets"
  )

})

test_that("variables not shared by both datasets are ignored", {

  # extra synth-only column must not create combinations
  synth_extra <- dplyr::mutate(synth, c = c("m", "m", "n", "n"))

  expect_equal(
    .util_k_marginals(synth_data = synth_extra, conf_data = conf, k = 1),
    .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)
  )

})

test_that("util_k_marginals accepts an eval_data object", {

  ed <- eval_data(conf_data = conf, synth_data = synth)

  expect_equal(util_k_marginals(eval_data = ed, k = 1), 750)

  expect_equal(util_k_marginals(eval_data = ed, k = 2), 5000 / 6)

})

test_that("util_k_marginals maps over replicates", {

  ed <- eval_data(conf_data = conf, synth_data = list(synth, conf))

  result <- util_k_marginals(eval_data = ed, k = 1)

  expect_equal(result, list(750, 1000))

})

test_that("util_k_marginals rejects non-eval_data input", {

  expect_error(util_k_marginals(eval_data = synth, k = 1))

})
