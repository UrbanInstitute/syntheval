test_that("cells report proportions and absolute differences per combo", {
  synth_cm <- tibble::tibble(
    a = c("x", "y", "y", "y"),
    b = c("p", "q", "q", "p")
  )

  conf_cm <- tibble::tibble(
    a = c("x", "x", "y", "y"),
    b = c("p", "p", "p", "q")
  )

  combos <- matrix(c("a", "b"), ncol = 1)

  cells <- .compute_marginal_cells(
    synth_data = synth_cm,
    conf_data = conf_cm,
    combos = combos
  )

  expect_named(
    cells,
    c("variables", "cell", "prop_synth", "prop_conf", "abs_diff")
  )

  # one row per observed level of each single-variable marginal
  expect_equal(nrow(cells), 4)
  expect_equal(unique(cells$variables), c("a", "b"))
})

# This test asserts two cases:
# 1. An empty synthetic marginal is allowed when `allow_empty_synth = TRUE`;
#    its cell proportions are treated as 0 and compared to the confidential
#    proportions.
# 2. An empty confidential marginal is always invalid, because there is no
#    observed reference distribution to score against, so
#    `.compute_marginal_cells()` should error.
test_that("only the synthetic side may be empty", {
  synth_empty <- tibble::tibble(a = c(NA_character_, NA_character_))
  conf_ok <- tibble::tibble(a = c("x", "y"))

  combos <- matrix("a", ncol = 1)

  # empty synthetic side scores against zero proportions
  cells <- .compute_marginal_cells(
    synth_data = synth_empty,
    conf_data = conf_ok,
    combos = combos,
    na.rm = TRUE,
    allow_empty_synth = TRUE
  )

  expect_equal(cells$prop_synth, c(0, 0))
  expect_equal(cells$prop_conf, c(0.5, 0.5))

  # the confidential side has nothing to score against and always errors
  expect_error(
    .compute_marginal_cells(
      synth_data = conf_ok,
      conf_data = synth_empty,
      combos = combos,
      na.rm = TRUE,
      allow_empty_synth = TRUE
    ),
    regexp = "no rows remain"
  )
})
