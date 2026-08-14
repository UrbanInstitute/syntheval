test_that("stratified results roll up by confidential shares", {
  # stratum A diverges (score 500), stratum B matches (score 1000);
  # equal confidential shares give an overall score of 750
  conf_st <- tibble::tibble(
    g = c("A", "A", "B", "B"),
    a = c("x", "y", "x", "y")
  )

  synth_st <- tibble::tibble(
    g = c("A", "A", "B", "B"),
    a = c("x", "x", "x", "y")
  )

  combos <- matrix("a", ncol = 1)

  result <- .stratify_k_marginals(
    synth_data = synth_st,
    conf_data = conf_st,
    combos = combos,
    group_by = "g"
  )

  expect_equal(result$score, 750)

  # group_scores are worst first with the grouping column attached
  expect_named(result$group_scores, c("g", "share", "score"))
  expect_equal(result$group_scores$g, c("A", "B"))
  expect_equal(result$group_scores$share, c(0.5, 0.5))

  # stacked detail tables carry the grouping column, worst first
  expect_equal(result$marginals$g[1], "A")
  expect_true("g" %in% names(result$cells))
})

test_that("stratum shares use weights when weight_var is set", {
  # conf weight shares: A = 0.5, B = 0.5 (row shares would be 2/3, 1/3)
  conf_stw <- tibble::tibble(
    g = c("A", "A", "B"),
    a = c("x", "y", "x"),
    w = c(1, 1, 2)
  )

  synth_stw <- tibble::tibble(
    g = c("A", "A", "B"),
    a = c("x", "x", "x"),
    w = c(1, 1, 1)
  )

  result <- .stratify_k_marginals(
    synth_data = synth_stw,
    conf_data = conf_stw,
    combos = matrix("a", ncol = 1),
    group_by = "g",
    weight_var = "w"
  )

  expect_equal(sort(result$group_scores$share), c(0.5, 0.5))
  expect_equal(result$score, 750)
})

test_that("multi-column group_by builds joint strata directly", {
  # only stratum (A, p) diverges: conf (x = 0.5, y = 0.5), synth (x = 1)
  # -> 500; the other three strata match -> 1000
  # overall = 0.25 * 500 + 0.75 * 1000 = 875
  conf_st2 <- tibble::tibble(
    g1 = c("A", "A", "A", "A", "B", "B", "B", "B"),
    g2 = c("p", "p", "q", "q", "p", "p", "q", "q"),
    a = c("x", "y", "x", "y", "x", "y", "x", "y")
  )

  synth_st2 <- dplyr::mutate(
    conf_st2,
    a = c("x", "x", "x", "y", "x", "y", "x", "y")
  )

  result <- .stratify_k_marginals(
    synth_data = synth_st2,
    conf_data = conf_st2,
    combos = matrix("a", ncol = 1),
    group_by = c("g1", "g2")
  )

  expect_equal(result$score, 875)
  expect_named(result$group_scores, c("g1", "g2", "share", "score"))
  expect_equal(nrow(result$group_scores), 4)
})
