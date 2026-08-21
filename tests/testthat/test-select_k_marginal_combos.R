# ---- fixtures ---------------------------------------------------------------
#
# three variables, k = 2: 3 combinations, 2 containing a
# four variables, k = 2: 6 combinations, 3 containing a

vars_3 <- c("a", "b", "c")
vars_4 <- c("a", "b", "c", "d")

has_a <- function(combos) {
  purrr::map_lgl(
    .x = seq_len(nrow(combos)),
    .f = \(i) "a" %in% combos[i, ]
  )
}

# ---- enumeration and sampling -----------------------------------------------

test_that("all combinations are enumerated when under the cap", {
  combos <- .select_k_marginal_combos(
    shared_vars = vars_3, k = 2, n_marginals = Inf
  )

  expect_equal(dim(combos), c(3, 2))
})

test_that("sampling is reproducible given a seed", {
  set.seed(1)
  first <- .select_k_marginal_combos(
    shared_vars = vars_4, k = 2, n_marginals = 2
  )

  set.seed(1)
  second <- .select_k_marginal_combos(
    shared_vars = vars_4, k = 2, n_marginals = 2
  )

  expect_equal(nrow(first), 2)
  expect_equal(first, second)
})

# ---- priority variables -----------------------------------------------------

test_that("priority combinations are always kept", {
  # the two combinations containing a fill the cap exactly
  combos <- .select_k_marginal_combos(
    shared_vars = vars_3,
    k = 2,
    n_marginals = 2,
    priority_vars = "a"
  )

  expect_equal(nrow(combos), 2)
  expect_true(all(has_a(combos)))

  # priority combinations exceeding the cap are still all kept
  combos_over <- .select_k_marginal_combos(
    shared_vars = vars_3,
    k = 2,
    n_marginals = 1,
    priority_vars = "a"
  )

  expect_equal(nrow(combos_over), 2)
  expect_true(all(has_a(combos_over)))
})

test_that("sampling fills remaining slots after priority combinations", {
  set.seed(20250813)

  combos <- .select_k_marginal_combos(
    shared_vars = vars_4,
    k = 2,
    n_marginals = 4,
    priority_vars = "a"
  )

  # all 3 priority combinations plus 1 sampled non-priority
  expect_equal(nrow(combos), 4)
  expect_equal(sum(has_a(combos)), 3)
})

test_that("non-priority sample size never exceeds the available slots", {
  is_priority <- c(TRUE, TRUE, FALSE, FALSE)

  # 4 slots, 2 priority -> 2 sampled
  expect_equal(
    .compute_nonpriority_sample_size(
      n_marginals = 4,
      is_priority = is_priority
    ),
    2
  )

  # 1 slot, 2 priority -> nothing sampled
  expect_equal(
    .compute_nonpriority_sample_size(
      n_marginals = 1,
      is_priority = is_priority
    ),
    0
  )

  # 10 slots, only 2 non-priority available -> 2 sampled
  expect_equal(
    .compute_nonpriority_sample_size(
      n_marginals = 10,
      is_priority = is_priority
    ),
    2
  )
})
