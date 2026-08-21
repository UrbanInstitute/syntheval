test_that("all combinations are enumerated when under the cap", {
  combos <- .select_k_marginal_combos(
    shared_vars = c("a", "b", "c"),
    k = 2,
    n_marginals = Inf
  )

  expect_equal(nrow(combos), 3)
  expect_equal(ncol(combos), 2)
})

test_that("priority combinations are always kept", {
  # the two combinations containing a fill the cap exactly, so the
  # selection is deterministic despite sampling
  combos <- .select_k_marginal_combos(
    shared_vars = c("a", "b", "c"),
    k = 2,
    n_marginals = 2,
    priority_vars = "a"
  )

  expect_equal(nrow(combos), 2)
  expect_true(all(
    purrr::map_lgl(
      .x = seq_len(nrow(combos)),
      .f = \(i) "a" %in% combos[i, ]
    )
  ))
})

test_that("priority combinations exceeding n_marginals are all kept", {
  combos <- .select_k_marginal_combos(
    shared_vars = c("a", "b", "c"),
    k = 2,
    n_marginals = 1,
    priority_vars = "a"
  )

  expect_equal(nrow(combos), 2)
  expect_true(all(
    purrr::map_lgl(
      .x = seq_len(nrow(combos)),
      .f = \(i) "a" %in% combos[i, ]
    )
  ))
})

test_that("sampling fills remaining slots after priority combinations", {
  # 4 variables, k = 2: 6 combinations, 3 containing a
  set.seed(20250813)

  combos <- .select_k_marginal_combos(
    shared_vars = c("a", "b", "c", "d"),
    k = 2,
    n_marginals = 4,
    priority_vars = "a"
  )

  # cap respected exactly: all 3 priority combos plus 1 sampled non-priority
  expect_equal(nrow(combos), 4)

  has_a <- purrr::map_lgl(
    .x = seq_len(nrow(combos)),
    .f = \(i) "a" %in% combos[i, ]
  )
  expect_equal(sum(has_a), 3)
  expect_equal(sum(!has_a), 1)
})
