# ---- fixtures ---------------------------------------------------------------
#
#   conf               synth
#     g  a               g  a
#     A  x               A  x
#     A  y               A  x
#     B  x               B  x
#     B  y               B  y
#
#   stratum A (conf share 0.5): conf (x = 0.5, y = 0.5), synth (x = 1)
#     MabsDD = 0.5 -> score 0.5
#   stratum B (conf share 0.5): identical -> score 0
#   overall = 0.5 * 0.5 + 0.5 * 0 = 0.25

conf <- tibble::tibble(
  g = c("A", "A", "B", "B"),
  a = c("x", "y", "x", "y")
)

synth <- dplyr::mutate(conf, a = c("x", "x", "x", "y"))

combos_a <- matrix("a", ncol = 1)

# ---- stratified results -----------------------------------------------------

test_that("stratified results roll up by confidential shares", {
  result <- .stratify_k_marginals(
    synth_data = synth,
    conf_data = conf,
    combos = combos_a,
    group_by = "g"
  )

  expect_equal(result$score, 0.25)

  # group_scores are worst first with the grouping column attached
  expect_named(result$group_scores, c("g", "share", "score"))
  expect_equal(result$group_scores$g, c("A", "B"))
  expect_equal(result$group_scores$share, c(0.5, 0.5))
  expect_equal(result$group_scores$score, c(0.5, 0))

  # stacked detail tables carry the grouping column, worst first
  expect_equal(result$marginals$g[1], "A")
  expect_true("g" %in% names(result$cells))
})

test_that("stratum shares use weights when weight_var is set", {
  # conf weights 1, 1, 2, 2 -> shares A = 1/3, B = 2/3 (row shares 0.5, 0.5)
  # overall = 1/3 * 0.5 + 2/3 * 0 = 1/6
  conf_weighted <- dplyr::mutate(conf, w = c(1, 1, 2, 2))
  synth_weighted <- dplyr::mutate(synth, w = 1)

  result <- .stratify_k_marginals(
    synth_data = synth_weighted,
    conf_data = conf_weighted,
    combos = combos_a,
    group_by = "g",
    weight_var = "w"
  )

  expect_equal(result$group_scores$share, c(1 / 3, 2 / 3))
  expect_equal(result$score, 1 / 6)
})

test_that("multi-column group_by builds joint strata", {
  # four joint strata of two rows each; only (A, p) diverges -> 0.5
  # overall = 0.25 * 0.5 + 0.75 * 0 = 0.125
  conf_joint <- tibble::tibble(
    g1 = c("A", "A", "A", "A", "B", "B", "B", "B"),
    g2 = c("p", "p", "q", "q", "p", "p", "q", "q"),
    a = c("x", "y", "x", "y", "x", "y", "x", "y")
  )

  synth_joint <- dplyr::mutate(
    conf_joint,
    a = c("x", "x", "x", "y", "x", "y", "x", "y")
  )

  result <- .stratify_k_marginals(
    synth_data = synth_joint,
    conf_data = conf_joint,
    combos = combos_a,
    group_by = c("g1", "g2")
  )

  expect_equal(result$score, 0.125)
  expect_named(result$group_scores, c("g1", "g2", "share", "score"))
  expect_equal(nrow(result$group_scores), 4)
})

# ---- missing values ---------------------------------------------------------

test_that("a confidential stratum emptied by na.rm errors instead of NaN", {
  # stratum B's confidential values of a are all missing, so per-marginal
  # NA removal leaves nothing to score against
  conf_na_stratum <- dplyr::mutate(conf, a = c("x", "y", NA, NA))

  expect_error(
    .stratify_k_marginals(
      synth_data = conf,
      conf_data = conf_na_stratum,
      combos = combos_a,
      group_by = "g",
      na.rm = TRUE
    ),
    regexp = "no rows remain"
  )

  # the synthetic side emptying in a stratum is allowed:
  # stratum B scores conf (x = 0.5, y = 0.5) against zero synth -> 0.5
  result <- .stratify_k_marginals(
    synth_data = conf_na_stratum,
    conf_data = conf,
    combos = combos_a,
    group_by = "g",
    na.rm = TRUE
  )

  expect_equal(result$score, 0.25)
})
