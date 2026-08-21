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
    .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)$score,
    750
  )
})

test_that("k = 2 score matches hand-computed value", {
  expect_equal(
    .util_k_marginals(synth_data = synth, conf_data = conf, k = 2)$score,
    5000 / 6
  )
})

test_that("identical data scores exactly 1000 for every k", {
  for (k in 1:2) {
    expect_equal(
      .util_k_marginals(synth_data = conf, conf_data = conf, k = k)$score,
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
    .util_k_marginals(synth_data = synth_gap, conf_data = conf_gap, k = 1)$score,
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
    regexp = "`k` cannot exceed the number of variables available"
  )
})

test_that("variables not shared by both datasets are ignored", {
  # extra synth-only column must not create combinations
  synth_extra <- dplyr::mutate(synth, c = c("m", "m", "n", "n"))

  expect_equal(
    .util_k_marginals(synth_data = synth_extra, conf_data = conf, k = 1)$score,
    .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)$score
  )
})

test_that("util_k_marginals accepts an eval_data object", {
  ed <- eval_data(conf_data = conf, synth_data = synth)

  expect_equal(util_k_marginals(eval_data = ed, k = 1)$score, 750)

  expect_equal(util_k_marginals(eval_data = ed, k = 2)$score, 5000 / 6)
})

test_that("util_k_marginals maps over replicates", {
  ed <- eval_data(conf_data = conf, synth_data = list(synth, conf))

  result <- util_k_marginals(eval_data = ed, k = 1)

  expect_equal(purrr::map_dbl(result, "score"), c(750, 1000))
})

test_that("util_k_marginals rejects non-eval_data input", {
  expect_error(util_k_marginals(eval_data = synth, k = 1))
})

test_that("marginals and cells report worst-first detail", {
  result <- .util_k_marginals(synth_data = synth, conf_data = conf, k = 2)

  expect_s3_class(result, "k_marginals")

  # single a x b combination with MabsDD = 1/6
  expect_equal(result$marginals$variables, "a, b")
  expect_equal(result$marginals$madd, 1 / 6)

  # three observed cells sorted by descending absolute difference
  expect_equal(
    names(result$cells),
    c("variables", "cell", "prop_synth", "prop_conf", "abs_diff", "diff")
  )
  expect_equal(result$cells$abs_diff, c(0.25, 0.25, 0))
  expect_equal(result$cells$cell[3], "y, p")
})

test_that("cells absent from the synthetic data appear with proportion zero", {
  conf_gap <- tibble::tibble(a = c("x", "y"))
  synth_gap <- tibble::tibble(a = c("x", "x"))

  result <- .util_k_marginals(synth_data = synth_gap, conf_data = conf_gap, k = 1)

  y_cell <- dplyr::filter(result$cells, .data$cell == "y")

  expect_equal(y_cell$prop_synth, 0)
  expect_equal(y_cell$prop_conf, 0.5)
})

test_that("print method reports the score and worst marginals", {
  result <- .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)

  expect_output(print(result), regexp = "k-marginals score: 750")
  expect_output(print(result), regexp = "Worst marginals:")
})

test_that("keep_marginals and keep_cells truncate the detail tables", {
  full <- .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)

  kept <- .util_k_marginals(
    synth_data = synth,
    conf_data = conf,
    k = 1,
    keep_marginals = 1,
    keep_cells = 2
  )

  expect_equal(nrow(kept$marginals), 1)
  expect_equal(nrow(kept$cells), 2)

  # retained rows are the worst ones from the full tables
  expect_equal(kept$marginals, full$marginals[1, ])
  expect_equal(kept$cells, full$cells[1:2, ])
})

test_that("retention keeps the highest abs_diff cells", {
  # three levels with a strict worst cell: y has the largest abs_diff
  # conf: x = 0.500, y = 0.250, z = 0.250
  # synth: x = 0.250, y = 0.625, z = 0.125
  # abs_diff: x = 0.250, y = 0.375, z = 0.125
  conf_tri <- tibble::tibble(a = c(rep("x", 4), rep("y", 2), rep("z", 2)))
  synth_tri <- tibble::tibble(a = c(rep("x", 2), rep("y", 5), "z"))

  kept <- .util_k_marginals(
    synth_data = synth_tri,
    conf_data = conf_tri,
    k = 1,
    keep_cells = 1
  )

  expect_equal(kept$cells$cell, "y")
  expect_equal(kept$cells$abs_diff, 0.375)
})

test_that("score is computed from all marginals, not the retained subset", {
  full <- .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)

  kept <- .util_k_marginals(
    synth_data = synth,
    conf_data = conf,
    k = 1,
    keep_marginals = 1,
    keep_cells = 1
  )

  expect_equal(kept$score, full$score)
})

test_that("invalid retention arguments throw an error", {
  for (bad_keep in list(0, "5", 1.5, NA_real_, NaN, c(1, 2))) {
    expect_error(
      .util_k_marginals(
        synth_data = synth, conf_data = conf, k = 1, keep_marginals = bad_keep
      ),
      regexp = "must be single integers >= 1 or Inf"
    )

    expect_error(
      .util_k_marginals(
        synth_data = synth, conf_data = conf, k = 1, keep_cells = bad_keep
      ),
      regexp = "must be single integers >= 1 or Inf"
    )
  }

  # Inf remains valid: it is the documented keep-everything default
  expect_equal(
    .util_k_marginals(
      synth_data = synth, conf_data = conf, k = 1,
      keep_marginals = Inf, keep_cells = Inf
    )$score,
    750
  )
})

test_that("util_k_marginals passes retention arguments through", {
  ed <- eval_data(conf_data = conf, synth_data = synth)

  result <- util_k_marginals(
    eval_data = ed, k = 1, keep_marginals = 1, keep_cells = 2
  )

  expect_equal(nrow(result$marginals), 1)
  expect_equal(nrow(result$cells), 2)
  expect_equal(result$score, 750)
})

test_that("non-integer and non-finite k values throw an error", {
  for (bad_k in list(1.5, NA_real_, NaN, Inf)) {
    expect_error(
      .util_k_marginals(synth_data = synth, conf_data = conf, k = bad_k),
      regexp = "`k` must be a single integer between 1 and 3"
    )
  }
})

test_that("marginals are sorted by descending madd across combinations", {
  # third shared variable, identical in conf but perturbed in synth
  # pair madds: (a, c) = 1/4, (a, b) = 1/6, (b, c) = 1/6
  conf_3 <- dplyr::mutate(conf, c = c("m", "m", "n", "n"))
  synth_3 <- dplyr::mutate(synth, c = c("m", "n", "n", "n"))

  result <- .util_k_marginals(synth_data = synth_3, conf_data = conf_3, k = 2)

  expect_equal(result$marginals$variables[1], "a, c")
  expect_equal(result$marginals$madd, c(1 / 4, 1 / 6, 1 / 6))

  # global cell ordering holds across cells from different combinations
  expect_equal(
    result$cells$abs_diff,
    sort(result$cells$abs_diff, decreasing = TRUE)
  )
})

test_that("each replicate result is a complete k_marginals object", {
  ed <- eval_data(conf_data = conf, synth_data = list(synth, conf))

  result <- util_k_marginals(eval_data = ed, k = 1)

  for (rep in result) {
    expect_s3_class(rep, "k_marginals")
    expect_named(rep, c("score", "marginals", "cells"))
    expect_gt(nrow(rep$marginals), 0)
    expect_gt(nrow(rep$cells), 0)
  }
})

test_that("conf-only extra columns are ignored", {
  conf_extra <- dplyr::mutate(conf, c = c("m", "m", "n", "n"))

  expect_equal(
    .util_k_marginals(synth_data = synth, conf_data = conf_extra, k = 1)$score,
    .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)$score
  )
})

test_that("one-row datasets produce a valid result", {
  one_row <- tibble::tibble(a = "x", b = "p")

  result <- .util_k_marginals(synth_data = one_row, conf_data = one_row, k = 1)

  expect_equal(result$score, 1000)
  expect_equal(nrow(result$cells), 2)
})

test_that("print truncates the marginals display to n rows", {
  result <- .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)

  out <- utils::capture.output(print(result, n = 1))

  # tibble rows print with a leading row number: row 1 only, no row 2
  expect_true(any(grepl("^1 ", out)))
  expect_false(any(grepl("^2 ", out)))
})

test_that("n_marginals caps the number of evaluated combinations", {
  conf_3 <- dplyr::mutate(conf, c = c("m", "m", "n", "n"))
  synth_3 <- dplyr::mutate(synth, c = c("m", "n", "n", "n"))

  set.seed(20250813)

  result <- .util_k_marginals(
    synth_data = synth_3, conf_data = conf_3, k = 2, n_marginals = 2
  )

  expect_equal(nrow(result$marginals), 2)
  expect_true(result$score >= 0 && result$score <= 1000)
})

test_that("sampling is reproducible given a seed", {
  conf_3 <- dplyr::mutate(conf, c = c("m", "m", "n", "n"))
  synth_3 <- dplyr::mutate(synth, c = c("m", "n", "n", "n"))

  set.seed(1)
  first <- .util_k_marginals(
    synth_data = synth_3, conf_data = conf_3, k = 2, n_marginals = 1
  )

  set.seed(1)
  second <- .util_k_marginals(
    synth_data = synth_3, conf_data = conf_3, k = 2, n_marginals = 1
  )

  expect_equal(first, second)
})

test_that("n_marginals at or above the combination count changes nothing", {
  conf_3 <- dplyr::mutate(conf, c = c("m", "m", "n", "n"))
  synth_3 <- dplyr::mutate(synth, c = c("m", "n", "n", "n"))

  full <- .util_k_marginals(synth_data = synth_3, conf_data = conf_3, k = 2)

  capped <- .util_k_marginals(
    synth_data = synth_3, conf_data = conf_3, k = 2, n_marginals = 3
  )

  expect_equal(capped, full)
})

test_that("invalid sampling arguments throw an error", {
  expect_error(
    .util_k_marginals(
      synth_data = synth, conf_data = conf, k = 1, n_marginals = 1.5
    ),
    regexp = "must be single integers >= 1 or Inf"
  )

  expect_error(
    .util_k_marginals(
      synth_data = synth, conf_data = conf, k = 1, priority_vars = "zzz"
    ),
    regexp = "`priority_vars` must be a character vector of variables available"
  )

  expect_error(
    .util_k_marginals(
      synth_data = synth, conf_data = conf, k = 1, priority_vars = 1
    ),
    regexp = "`priority_vars` must be a character vector of variables available"
  )
})

test_that("util_k_marginals passes sampling arguments through", {
  conf_3 <- dplyr::mutate(conf, c = c("m", "m", "n", "n"))
  synth_3 <- dplyr::mutate(synth, c = c("m", "n", "n", "n"))

  ed <- eval_data(conf_data = conf_3, synth_data = synth_3)

  result <- util_k_marginals(
    eval_data = ed, k = 2, n_marginals = 2, priority_vars = "a"
  )

  expect_equal(sort(result$marginals$variables), c("a, b", "a, c"))
})

test_that("n_marginals caps k = 3 combinations", {
  # 4 shared variables, k = 3: 4 combinations
  conf_4 <- dplyr::mutate(
    conf,
    c = c("m", "m", "n", "n"), d = c("u", "v", "u", "v")
  )
  synth_4 <- dplyr::mutate(
    synth,
    c = c("m", "n", "n", "n"), d = c("v", "v", "u", "u")
  )

  set.seed(20250813)

  result <- .util_k_marginals(
    synth_data = synth_4, conf_data = conf_4, k = 3, n_marginals = 2
  )

  expect_equal(nrow(result$marginals), 2)
  expect_true(result$score >= 0 && result$score <= 1000)
})

test_that("priority_vars applies to k = 3 combinations", {
  # priority a appears in 3 of the 4 triples; cap of 3 keeps exactly those
  conf_4 <- dplyr::mutate(
    conf,
    c = c("m", "m", "n", "n"), d = c("u", "v", "u", "v")
  )
  synth_4 <- dplyr::mutate(
    synth,
    c = c("m", "n", "n", "n"), d = c("v", "v", "u", "u")
  )

  result <- .util_k_marginals(
    synth_data = synth_4,
    conf_data = conf_4,
    k = 3,
    n_marginals = 3,
    priority_vars = "a"
  )

  expect_equal(
    sort(result$marginals$variables),
    c("a, b, c", "a, b, d", "a, c, d")
  )
})

test_that("weighted proportions match hand-computed values", {
  # conf: weight shares x = 3/4, y = 1/4; synth: x = 1/2, y = 1/2
  # MabsDD = mean(0.25, 0.25) = 0.25 -> score 750
  conf_w <- tibble::tibble(a = c("x", "y"), w = c(3, 1))
  synth_w <- tibble::tibble(a = c("x", "y"), w = c(1, 1))

  result <- .util_k_marginals(
    synth_data = synth_w, conf_data = conf_w, k = 1, weight_var = "w"
  )

  expect_equal(result$score, 750)
  expect_equal(
    dplyr::filter(result$cells, .data$cell == "x")$prop_conf,
    0.75
  )
})

test_that("unit weights reproduce the unweighted result", {
  conf_w <- dplyr::mutate(conf, w = 1)
  synth_w <- dplyr::mutate(synth, w = 1)

  weighted <- .util_k_marginals(
    synth_data = synth_w, conf_data = conf_w, k = 1, weight_var = "w"
  )

  unweighted <- .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)

  expect_equal(weighted, unweighted)
})

test_that("the weight column is never a marginal", {
  conf_w <- dplyr::mutate(conf, w = 1)
  synth_w <- dplyr::mutate(synth, w = 1)

  result <- .util_k_marginals(
    synth_data = synth_w, conf_data = conf_w, k = 1, weight_var = "w"
  )

  expect_equal(sort(result$marginals$variables), c("a", "b"))
})

test_that("invalid weight_var throws an error", {
  conf_w <- dplyr::mutate(conf, w = 1)
  synth_w <- dplyr::mutate(synth, w = 1)

  expect_error(
    .util_k_marginals(
      synth_data = synth_w, conf_data = conf_w, k = 1, weight_var = 1
    ),
    regexp = "`weight_var` must be a single character string"
  )

  # column missing from one dataset
  expect_error(
    .util_k_marginals(
      synth_data = synth, conf_data = conf_w, k = 1, weight_var = "w"
    ),
    regexp = "`weight_var` must be a column in both datasets"
  )

  # non-numeric weight column
  conf_chr <- dplyr::mutate(conf, w = "1")
  synth_chr <- dplyr::mutate(synth, w = "1")

  expect_error(
    .util_k_marginals(
      synth_data = synth_chr, conf_data = conf_chr, k = 1, weight_var = "w"
    ),
    regexp = "`weight_var` must be a numeric column in both datasets"
  )
})

test_that("util_k_marginals passes weight_var through", {
  conf_w <- tibble::tibble(a = c("x", "y"), w = c(3, 1))
  synth_w <- tibble::tibble(a = c("x", "y"), w = c(1, 1))

  ed <- eval_data(conf_data = conf_w, synth_data = synth_w)

  expect_equal(
    util_k_marginals(eval_data = ed, k = 1, weight_var = "w")$score,
    750
  )
})

test_that("invalid weight values throw an error", {
  synth_w <- dplyr::mutate(synth, w = 1)

  bad_weights <- list(
    c(1, 1, 1, -1), # negative
    c(1, 1, 1, NA), # missing
    c(1, 1, 1, Inf), # non-finite
    c(0, 0, 0, 0) # zero total
  )

  for (bad_w in bad_weights) {
    conf_bad <- dplyr::mutate(conf, w = bad_w)

    expect_error(
      .util_k_marginals(
        synth_data = synth_w, conf_data = conf_bad, k = 1, weight_var = "w"
      ),
      regexp = "finite and non-negative with a positive total"
    )

    # symmetric: same weights are rejected on the synthetic side
    synth_bad <- dplyr::mutate(synth, w = bad_w)
    conf_w <- dplyr::mutate(conf, w = 1)

    expect_error(
      .util_k_marginals(
        synth_data = synth_bad, conf_data = conf_w, k = 1, weight_var = "w"
      ),
      regexp = "finite and non-negative with a positive total"
    )
  }
})

test_that("zero weights are valid when the total is positive", {
  # zero-weight rows drop out: conf weight shares x = 1, y = 0
  conf_w <- tibble::tibble(a = c("x", "y"), w = c(2, 0))
  synth_w <- tibble::tibble(a = c("x", "y"), w = c(1, 1))

  result <- .util_k_marginals(
    synth_data = synth_w, conf_data = conf_w, k = 1, weight_var = "w"
  )

  expect_equal(result$score, 500)
})

test_that("width discretization matches hand-computed values", {
  # conf 1:4 with 2 bins: interior cut at 2.5, so low = {1, 2}, high = {3, 4}
  # conf shares (0.5, 0.5); synth c(1, 1, 1, 4) shares (0.75, 0.25)
  # MabsDD = mean(0.25, 0.25) = 0.25 -> score 750
  conf_num <- tibble::tibble(v = c(1, 2, 3, 4))
  synth_num <- tibble::tibble(v = c(1, 1, 1, 4))

  result <- .util_k_marginals(
    synth_data = synth_num, conf_data = conf_num, k = 1, bins = 2
  )

  expect_equal(result$score, 750)
  expect_equal(nrow(result$cells), 2)
})

test_that("ntile discretization uses confidential quantiles", {
  # conf quartile cut points at 25/50/75th percentiles of 1:8
  # 4 bins of 2 values each: conf shares 0.25 apiece
  # synth all in the lowest bin: shares (1, 0, 0, 0)
  # MabsDD = mean(0.75, 0.25, 0.25, 0.25) = 0.375 -> score 625
  conf_num <- tibble::tibble(v = c(1, 2, 3, 4, 5, 6, 7, 8))
  synth_num <- tibble::tibble(v = c(1, 1, 1, 1, 1, 1, 1, 1))

  result <- .util_k_marginals(
    synth_data = synth_num,
    conf_data = conf_num,
    k = 1,
    bins = 4,
    discretize_method = "ntile"
  )

  expect_equal(result$score, 625)
})

test_that("cluster discretization separates well-separated groups", {
  # two tight clusters around 1 and 10: the midpoint break lands between
  # them, so conf shares (0.5, 0.5) and synth (1, 0) -> score 500
  conf_num <- tibble::tibble(v = c(1, 1.1, 10, 10.1))
  synth_num <- tibble::tibble(v = c(1, 1, 1.1, 1.1))

  set.seed(20250813)

  result <- .util_k_marginals(
    synth_data = synth_num,
    conf_data = conf_num,
    k = 1,
    bins = 2,
    discretize_method = "cluster"
  )

  expect_equal(result$score, 500)
})

test_that("synthetic values outside the confidential range land in edge bins", {
  conf_num <- tibble::tibble(v = c(1, 2, 3, 4))
  synth_num <- tibble::tibble(v = c(-100, -100, 100, 100))

  result <- .util_k_marginals(
    synth_data = synth_num, conf_data = conf_num, k = 1, bins = 2
  )

  # extremes split evenly across the two edge bins, matching conf shares
  expect_equal(result$score, 1000)
})

test_that("non-numeric variables are untouched by discretization", {
  conf_mix <- dplyr::mutate(conf, v = c(1, 2, 3, 4))
  synth_mix <- dplyr::mutate(synth, v = c(1, 2, 3, 4))

  result <- .util_k_marginals(
    synth_data = synth_mix, conf_data = conf_mix, k = 1, bins = 2
  )

  # categorical marginals a and b keep their original levels
  a_cells <- dplyr::filter(result$cells, .data$variables == "a")
  expect_equal(sort(a_cells$cell), c("x", "y"))
})

test_that("bins = NULL leaves numeric variables as-is", {
  conf_num <- tibble::tibble(v = c(1, 2, 3, 4))
  synth_num <- tibble::tibble(v = c(1, 1, 1, 4))

  result <- .util_k_marginals(
    synth_data = synth_num, conf_data = conf_num, k = 1
  )

  # every distinct value is its own cell
  expect_equal(nrow(result$cells), 4)
})

test_that("invalid discretization arguments reach the helper's errors", {
  # bad-bins shapes are covered in test-discretize_k_marginal_vars.R; one
  # case here confirms the worker wires bins through to that validation
  conf_num <- tibble::tibble(v = c(1, 2, 3, 4))
  synth_num <- tibble::tibble(v = c(1, 1, 1, 4))

  expect_error(
    .util_k_marginals(
      synth_data = synth_num, conf_data = conf_num, k = 1, bins = 1
    ),
    regexp = "`bins` must be a single integer >= 2"
  )

  expect_error(
    .util_k_marginals(
      synth_data = synth_num, conf_data = conf_num, k = 1, bins = 2,
      discretize_method = "magic"
    )
  )
})

test_that("util_k_marginals passes discretization arguments through", {
  conf_num <- tibble::tibble(v = c(1, 2, 3, 4))
  synth_num <- tibble::tibble(v = c(1, 1, 1, 4))

  ed <- eval_data(conf_data = conf_num, synth_data = synth_num)

  expect_equal(
    suppressMessages(util_k_marginals(eval_data = ed, k = 1, bins = 2))$score,
    750
  )
})

test_that("util_k_marginals messages the resolved discretization method", {
  conf_num <- tibble::tibble(v = c(1, 2, 3, 4))
  synth_num <- tibble::tibble(v = c(1, 1, 1, 4))

  ed <- eval_data(conf_data = conf_num, synth_data = synth_num)

  expect_message(
    util_k_marginals(eval_data = ed, k = 1, bins = 2),
    regexp = "2 bins using the 'width' method"
  )

  expect_message(
    util_k_marginals(
      eval_data = ed, k = 1, bins = 2, discretize_method = "ntile"
    ),
    regexp = "2 bins using the 'ntile' method"
  )

  # no discretization, no message
  expect_no_message(util_k_marginals(eval_data = ed, k = 1))
})

test_that("synth_varnames restricts the worker's variable universe", {
  conf_sv <- tibble::tibble(
    a = c("x", "x", "y", "y"),
    b = c("p", "p", "p", "q"),
    c = c("m", "n", "m", "n")
  )

  synth_sv <- tibble::tibble(
    a = c("x", "y", "y", "y"),
    b = c("p", "q", "q", "p"),
    c = c("m", "n", "m", "n")
  )

  result <- .util_k_marginals(
    synth_data = synth_sv,
    conf_data = conf_sv,
    k = 2,
    synth_varnames = c("a", "b")
  )

  expect_equal(result$marginals$variables, "a, b")

  # NULL means no restriction
  result_all <- .util_k_marginals(
    synth_data = synth_sv,
    conf_data = conf_sv,
    k = 2,
    synth_varnames = NULL
  )

  expect_setequal(
    result_all$marginals$variables,
    c("a, b", "a, c", "b, c")
  )
})

test_that("synth_vars = TRUE keeps only synthesized variables for postsynth", {
  ed <- eval_data(
    conf_data = penguins_conf,
    synth_data = penguins_postsynth
  )

  result <- util_k_marginals(eval_data = ed, k = 1, synth_vars = TRUE)

  # species and island are carried over from start_data, not synthesized
  expect_setequal(result$marginals$variables, ed$synth_vars)
})

test_that("synth_vars = FALSE includes carried-over variables", {
  ed <- eval_data(
    conf_data = penguins_conf,
    synth_data = penguins_postsynth
  )

  result <- util_k_marginals(eval_data = ed, k = 1, synth_vars = FALSE)

  expect_setequal(
    result$marginals$variables,
    intersect(names(ed$conf_data), names(ed$synth_data))
  )

  # penguins_postsynth's start data was sampled, so the carried-over
  # variables have their own discrepancies; including them must change the
  # score, guarding against the flag being silently ignored
  result_synth_only <- util_k_marginals(
    eval_data = ed, k = 1, synth_vars = TRUE
  )

  expect_false(isTRUE(all.equal(result$score, result_synth_only$score)))
})

test_that("synth_vars = TRUE is a no-op for plain data frame eval_data", {
  conf_sv <- tibble::tibble(
    a = c("x", "x", "y", "y"),
    b = c("p", "p", "p", "q")
  )

  synth_sv <- tibble::tibble(
    a = c("x", "y", "y", "y"),
    b = c("p", "q", "q", "p")
  )

  ed <- eval_data(conf_data = conf_sv, synth_data = synth_sv)

  result <- util_k_marginals(eval_data = ed, k = 1, synth_vars = TRUE)

  expect_setequal(result$marginals$variables, c("a", "b"))
})

test_that("priority_vars excluded by synth_varnames error informatively", {
  conf_sv <- tibble::tibble(
    a = c("x", "x", "y", "y"),
    b = c("p", "p", "p", "q"),
    c = c("m", "n", "m", "n")
  )

  synth_sv <- tibble::tibble(
    a = c("x", "y", "y", "y"),
    b = c("p", "q", "q", "p"),
    c = c("m", "n", "m", "n")
  )

  expect_error(
    .util_k_marginals(
      synth_data = synth_sv,
      conf_data = conf_sv,
      k = 1,
      priority_vars = "c",
      synth_varnames = c("a", "b")
    ),
    regexp = "`priority_vars` must be a character vector of variables available"
  )
})

test_that("k is validated against the restricted variable universe", {
  conf_sv <- tibble::tibble(
    a = c("x", "x", "y", "y"),
    b = c("p", "p", "p", "q"),
    c = c("m", "n", "m", "n")
  )

  synth_sv <- tibble::tibble(
    a = c("x", "y", "y", "y"),
    b = c("p", "q", "q", "p"),
    c = c("m", "n", "m", "n")
  )

  expect_error(
    .util_k_marginals(
      synth_data = synth_sv,
      conf_data = conf_sv,
      k = 3,
      synth_varnames = c("a", "b")
    ),
    regexp = "`k` cannot exceed"
  )
})

test_that("invalid synth_vars values error", {
  conf_sv <- tibble::tibble(
    a = c("x", "x", "y", "y"),
    b = c("p", "p", "p", "q")
  )

  synth_sv <- tibble::tibble(
    a = c("x", "y", "y", "y"),
    b = c("p", "q", "q", "p")
  )

  ed <- eval_data(conf_data = conf_sv, synth_data = synth_sv)

  for (bad in list("x", c(TRUE, FALSE), NA, 1)) {
    expect_error(
      util_k_marginals(eval_data = ed, k = 1, synth_vars = bad),
      regexp = "`synth_vars` must be a single TRUE or FALSE"
    )
  }
})

test_that("invalid synth_varnames values error", {
  conf_sv <- tibble::tibble(
    a = c("x", "x", "y", "y"),
    b = c("p", "p", "p", "q")
  )

  synth_sv <- tibble::tibble(
    a = c("x", "y", "y", "y"),
    b = c("p", "q", "q", "p")
  )

  for (bad in list(character(0), NA_character_, c("a", NA), 1)) {
    expect_error(
      .util_k_marginals(
        synth_data = synth_sv,
        conf_data = conf_sv,
        k = 1,
        synth_varnames = bad
      ),
      regexp = "`synth_varnames` must be a non-empty character vector"
    )
  }
})

test_that("synth_varnames with no shared variables errors informatively", {
  conf_sv <- tibble::tibble(
    a = c("x", "x", "y", "y"),
    b = c("p", "p", "p", "q")
  )

  synth_sv <- tibble::tibble(
    a = c("x", "y", "y", "y"),
    b = c("p", "q", "q", "p")
  )

  expect_error(
    .util_k_marginals(
      synth_data = synth_sv,
      conf_data = conf_sv,
      k = 1,
      synth_varnames = "zzz"
    ),
    regexp = "`synth_varnames` matches no variables available"
  )
})

test_that("empty synthesized-variable metadata errors informatively", {
  conf_sv <- tibble::tibble(
    a = c("x", "x", "y", "y"),
    b = c("p", "p", "p", "q")
  )

  synth_sv <- tibble::tibble(
    a = c("x", "y", "y", "y"),
    b = c("p", "q", "q", "p")
  )

  ed <- eval_data(
    conf_data = conf_sv,
    synth_data = synth_sv,
    synth_vars = character(0)
  )

  expect_error(
    util_k_marginals(eval_data = ed, k = 1, synth_vars = TRUE),
    regexp = "records no synthesized variables"
  )

  # synth_vars = FALSE ignores the empty metadata
  expect_no_error(util_k_marginals(eval_data = ed, k = 1, synth_vars = FALSE))
})

test_that("wrapper restriction bounds k by the synthesized-variable set", {
  conf_sv <- tibble::tibble(
    a = c("x", "x", "y", "y"),
    b = c("p", "p", "p", "q")
  )

  synth_sv <- tibble::tibble(
    a = c("x", "y", "y", "y"),
    b = c("p", "q", "q", "p")
  )

  ed <- eval_data(
    conf_data = conf_sv,
    synth_data = synth_sv,
    synth_vars = "a"
  )

  expect_error(
    util_k_marginals(eval_data = ed, k = 2, synth_vars = TRUE),
    regexp = "`k` cannot exceed"
  )

  # the same call succeeds once the restriction is lifted
  expect_no_error(util_k_marginals(eval_data = ed, k = 2, synth_vars = FALSE))
})

# NA handling
#
# conf_na            synth_na
#   a   b              a   b
#   x   p              x   p
#   x   p              y   q
#   y   p              y   q
#   NA  q              NA  p
#
# na.rm = FALSE, k = 1 (NA is its own level)
#   marginal a: conf (x = 0.50, y = 0.25, NA = 0.25),
#               synth (x = 0.25, y = 0.50, NA = 0.25)
#     MabsDD = mean(0.25, 0.25, 0) = 1/6
#   marginal b: MabsDD = mean(0.25, 0.25) = 0.25
#   score = (1 - mean(1/6, 1/4)) * 1000 = 19000/24
#
# na.rm = TRUE, k = 1 (rows dropped per marginal)
#   marginal a (3 rows each): conf (x = 2/3, y = 1/3),
#                             synth (x = 1/3, y = 2/3)
#     MabsDD = mean(1/3, 1/3) = 1/3
#   marginal b (all 4 rows): MabsDD = 0.25
#   score = (1 - mean(1/3, 1/4)) * 1000 = 17000/24

conf_na <- tibble::tibble(
  a = c("x", "x", "y", NA),
  b = c("p", "p", "p", "q")
)

synth_na <- tibble::tibble(
  a = c("x", "y", "y", NA),
  b = c("p", "q", "q", "p")
)

test_that("NA values form their own level by default", {
  result <- suppressMessages(
    .util_k_marginals(synth_data = synth_na, conf_data = conf_na, k = 1)
  )

  expect_equal(result$score, 19000 / 24)

  expect_true("NA" %in% result$cells$cell)
})

test_that("na.rm = TRUE drops missing values per marginal", {
  result <- .util_k_marginals(
    synth_data = synth_na, conf_data = conf_na, k = 1, na.rm = TRUE
  )

  expect_equal(result$score, 17000 / 24)

  expect_false("NA" %in% result$cells$cell)
})

test_that("missing data triggers a message when na.rm = FALSE", {
  expect_message(
    .util_k_marginals(synth_data = synth_na, conf_data = conf_na, k = 1),
    regexp = "contain missing data: a"
  )

  expect_no_message(
    .util_k_marginals(
      synth_data = synth_na, conf_data = conf_na, k = 1, na.rm = TRUE
    )
  )
})

test_that("invalid na.rm values error", {
  for (bad in list("x", c(TRUE, FALSE), NA, 1)) {
    expect_error(
      .util_k_marginals(
        synth_data = synth_na, conf_data = conf_na, k = 1, na.rm = bad
      ),
      regexp = "`na.rm` must be a single TRUE or FALSE"
    )
  }
})

test_that("a literal 'NA' level alongside true NA values errors", {
  conf_lit <- tibble::tibble(a = c("NA", "x", NA))
  synth_lit <- tibble::tibble(a = c("x", "x", "x"))

  expect_error(
    suppressMessages(
      .util_k_marginals(synth_data = synth_lit, conf_data = conf_lit, k = 1)
    ),
    regexp = "'NA' already exists"
  )
})

test_that("numeric NA values land in an NA bin or are dropped", {
  conf_num <- tibble::tibble(v = c(1, 2, 3, 4))
  synth_num <- tibble::tibble(v = c(1, 4, NA, NA))

  kept <- suppressMessages(
    .util_k_marginals(
      synth_data = synth_num, conf_data = conf_num, k = 1, bins = 2
    )
  )

  # cut() maps synthetic NAs to an NA bin that becomes its own level
  expect_true("NA" %in% kept$cells$cell)

  dropped <- .util_k_marginals(
    synth_data = synth_num, conf_data = conf_num, k = 1, bins = 2,
    na.rm = TRUE
  )

  expect_false("NA" %in% dropped$cells$cell)

  # with the NAs dropped, the two synthetic values split evenly like the
  # confidential data, so the marginal matches exactly
  expect_equal(dropped$score, 1000)
})

test_that("a marginal with no complete rows errors under na.rm = TRUE", {
  conf_all_na <- tibble::tibble(a = c(NA_character_, NA_character_))
  synth_ok <- tibble::tibble(a = c("x", "y"))

  expect_error(
    .util_k_marginals(
      synth_data = synth_ok, conf_data = conf_all_na, k = 1, na.rm = TRUE
    ),
    regexp = "no rows remain"
  )
})

test_that("util_k_marginals passes na.rm through", {
  ed <- eval_data(conf_data = conf_na, synth_data = synth_na)

  expect_equal(
    util_k_marginals(eval_data = ed, k = 1, na.rm = TRUE)$score,
    17000 / 24
  )
})

test_that("na.rm = TRUE drops rows per combination, not globally", {
  # a's NA sits on a different row in each dataset; b and c are identical
  # across datasets
  #
  # pairwise deletion, k = 2:
  #   (b, c): all 4 rows, identical -> MabsDD = 0
  #   (a, b): conf drops row 4 -> (x,p), (x,q), (y,p) each 1/3
  #           synth drops row 3 -> (x,p), (x,q), (y,q) each 1/3
  #           MabsDD = mean(0, 0, 1/3, 1/3) = 1/6
  #   (a, c): conf -> (x,m) = 2/3, (y,n) = 1/3; synth identical -> MabsDD = 0
  #   score = (1 - mean(0, 1/6, 0)) * 1000 = 17000/18
  #
  # global (listwise) deletion would filter different rows from each dataset
  # and corrupt the complete (b, c) marginal, so madd = 0 for (b, c) is the
  # discriminating assertion
  conf_pair <- tibble::tibble(
    a = c("x", "x", "y", NA),
    b = c("p", "q", "p", "q"),
    c = c("m", "m", "n", "n")
  )

  synth_pair <- tibble::tibble(
    a = c("x", "x", NA, "y"),
    b = c("p", "q", "p", "q"),
    c = c("m", "m", "n", "n")
  )

  result <- .util_k_marginals(
    synth_data = synth_pair, conf_data = conf_pair, k = 2, na.rm = TRUE
  )

  expect_equal(result$score, 17000 / 18)

  bc <- dplyr::filter(result$marginals, .data$variables == "b, c")

  expect_equal(bc$madd, 0)
})

test_that("the missing-data message lists every affected variable", {
  conf_two <- tibble::tibble(
    a = c("x", NA),
    b = c(NA, "q"),
    c = c("m", "n")
  )

  synth_two <- tibble::tibble(
    a = c("x", "y"),
    b = c("p", "q"),
    c = c("m", "n")
  )

  expect_message(
    .util_k_marginals(synth_data = synth_two, conf_data = conf_two, k = 1),
    regexp = "contain missing data: a, b"
  )
})

test_that("a literal 'NA' level in the synthetic data also errors", {
  conf_lit <- tibble::tibble(a = c("x", "x", "x"))
  synth_lit <- tibble::tibble(a = c("NA", "x", NA))

  expect_error(
    suppressMessages(
      .util_k_marginals(synth_data = synth_lit, conf_data = conf_lit, k = 1)
    ),
    regexp = "'NA' already exists"
  )

  # both datasets carrying the collision still errors
  expect_error(
    suppressMessages(
      .util_k_marginals(synth_data = synth_lit, conf_data = synth_lit, k = 1)
    ),
    regexp = "'NA' already exists"
  )
})

test_that("variables excluded by synth_varnames do not drive NA handling", {
  # a has missing values but is filtered out, so no message and no NA cells
  conf_excl <- tibble::tibble(
    a = c("x", NA),
    b = c("p", "q")
  )

  synth_excl <- tibble::tibble(
    a = c(NA, "y"),
    b = c("p", "p")
  )

  expect_no_message(
    result <- .util_k_marginals(
      synth_data = synth_excl,
      conf_data = conf_excl,
      k = 1,
      synth_varnames = "b"
    )
  )

  expect_false("NA" %in% result$cells$cell)
})

test_that("weighted proportions drop missing rows before computing shares", {
  # na.rm = TRUE drops each dataset's NA row, and weight shares are computed
  # from the surviving rows' weights:
  #   conf keeps weights 1, 1, 2 -> x = 2/4, y = 2/4
  #   synth keeps weights 1, 3 -> x = 1/4, y = 3/4
  #   MabsDD = mean(0.25, 0.25) = 0.25 -> score 750
  # dividing by the full weight total (including the dropped 10s) would give
  # a different score, so 750 pins down the recomputation
  conf_w <- tibble::tibble(
    a = c("x", "x", "y", NA),
    w = c(1, 1, 2, 10)
  )

  synth_w <- tibble::tibble(
    a = c("x", "y", NA),
    w = c(1, 3, 10)
  )

  result <- .util_k_marginals(
    synth_data = synth_w,
    conf_data = conf_w,
    k = 1,
    weight_var = "w",
    na.rm = TRUE
  )

  expect_equal(result$score, 750)
})

test_that("confidential numeric NA values discretize under both na.rm modes", {
  # breaks derive from the observed confidential values (1:4, cut at 2.5)
  conf_num_na <- tibble::tibble(v = c(1, 2, 3, 4, NA))
  synth_num_na <- tibble::tibble(v = c(1, 2, 4, 4, NA))

  # na.rm = FALSE: both datasets bin as low 2/5, high 2/5, NA 1/5
  kept <- suppressMessages(
    .util_k_marginals(
      synth_data = synth_num_na, conf_data = conf_num_na, k = 1, bins = 2
    )
  )

  expect_true("NA" %in% kept$cells$cell)
  expect_equal(kept$score, 1000)

  # na.rm = TRUE: observed values bin as low 2/4, high 2/4 in both datasets
  dropped <- .util_k_marginals(
    synth_data = synth_num_na, conf_data = conf_num_na, k = 1, bins = 2,
    na.rm = TRUE
  )

  expect_false("NA" %in% dropped$cells$cell)
  expect_equal(dropped$score, 1000)
})

# group_by stratification
#
# conf_g             synth_g
#   g  a               g  a
#   A  x               A  x
#   A  y               A  x
#   B  x               B  x
#   B  y               B  y
#
# universe = {a}; g stratifies and never marginalizes
# stratum A (conf share 0.5): conf (x = 0.5, y = 0.5), synth (x = 1)
#   MabsDD = mean(0.5, 0.5) = 0.5 -> score 500
# stratum B (conf share 0.5): identical -> MabsDD = 0 -> score 1000
# overall = 0.5 * 500 + 0.5 * 1000 = 750

conf_g <- tibble::tibble(
  g = c("A", "A", "B", "B"),
  a = c("x", "y", "x", "y")
)

synth_g <- tibble::tibble(
  g = c("A", "A", "B", "B"),
  a = c("x", "x", "x", "y")
)

test_that("group_by stratifies the score by confidential shares", {
  result <- .util_k_marginals(
    synth_data = synth_g, conf_data = conf_g, k = 1, group_by = "g"
  )

  expect_equal(result$score, 750)
})

test_that("grouped output gains group columns and group_scores", {
  result <- .util_k_marginals(
    synth_data = synth_g, conf_data = conf_g, k = 1, group_by = "g"
  )

  expect_named(result$group_scores, c("g", "share", "score"))

  # worst stratum first
  expect_equal(result$group_scores$g, c("A", "B"))
  expect_equal(result$group_scores$share, c(0.5, 0.5))
  expect_equal(result$group_scores$score, c(500, 1000))

  expect_true("g" %in% names(result$marginals))
  expect_true("g" %in% names(result$cells))

  # worst-first ordering across strata
  expect_equal(result$marginals$g[1], "A")

  # ungrouped results carry no group_scores element
  ungrouped <- .util_k_marginals(synth_data = synth_g, conf_data = conf_g, k = 1)

  expect_null(ungrouped$group_scores)
})

test_that("an empty synthetic stratum scores against zero proportions", {
  # synth has no B rows: stratum B conf cells (x = 0.5, y = 0.5) face
  # synthetic proportions of 0 -> MabsDD = 0.5 -> score 500
  # stratum A: conf (x = 0.5, y = 0.5), synth (x = 0.5, y = 0.5) -> 1000
  # overall = 0.5 * 1000 + 0.5 * 500 = 750
  synth_a_only <- tibble::tibble(
    g = c("A", "A", "A", "A"),
    a = c("x", "x", "y", "y")
  )

  result <- .util_k_marginals(
    synth_data = synth_a_only, conf_data = conf_g, k = 1, group_by = "g"
  )

  expect_equal(result$score, 750)
})

test_that("group shares use weights when weight_var is set", {
  # conf weight shares: A = 2/4, B = 2/4 (row shares would be 2/3, 1/3)
  # stratum A: conf (x = 0.5, y = 0.5), synth (x = 1) -> score 500
  # stratum B: conf (x = 1), synth (x = 1) -> score 1000
  # overall = 0.5 * 500 + 0.5 * 1000 = 750; row shares would give 2000/3
  conf_gw <- tibble::tibble(
    g = c("A", "A", "B"),
    a = c("x", "y", "x"),
    w = c(1, 1, 2)
  )

  synth_gw <- tibble::tibble(
    g = c("A", "A", "B"),
    a = c("x", "x", "x"),
    w = c(1, 1, 1)
  )

  result <- .util_k_marginals(
    synth_data = synth_gw,
    conf_data = conf_gw,
    k = 1,
    group_by = "g",
    weight_var = "w"
  )

  expect_equal(result$score, 750)
})

test_that("invalid group_by values error", {
  expect_error(
    .util_k_marginals(
      synth_data = synth_g, conf_data = conf_g, k = 1, group_by = 1
    ),
    regexp = "`group_by` must be a character vector"
  )

  expect_error(
    .util_k_marginals(
      synth_data = synth_g, conf_data = conf_g, k = 1, group_by = "zzz"
    ),
    regexp = "`group_by` must be a character vector"
  )

  conf_w <- dplyr::mutate(conf_g, w = 1)
  synth_w <- dplyr::mutate(synth_g, w = 1)

  expect_error(
    .util_k_marginals(
      synth_data = synth_w, conf_data = conf_w, k = 1,
      group_by = "w", weight_var = "w"
    ),
    regexp = "`group_by` cannot include `weight_var`"
  )
})

test_that("group variables are excluded from the marginal universe", {
  result <- .util_k_marginals(
    synth_data = synth_g, conf_data = conf_g, k = 1, group_by = "g"
  )

  expect_false(any(result$marginals$variables == "g"))

  # k is checked against the universe without the group variables
  expect_error(
    .util_k_marginals(
      synth_data = synth_g, conf_data = conf_g, k = 2, group_by = "g"
    ),
    regexp = "`k` cannot exceed"
  )
})

test_that("grouped print shows group scores", {
  result <- .util_k_marginals(
    synth_data = synth_g, conf_data = conf_g, k = 1, group_by = "g"
  )

  expect_output(print(result), regexp = "Worst groups:")
})

test_that("util_k_marginals passes group_by through", {
  ed <- eval_data(conf_data = conf_g, synth_data = synth_g)

  expect_equal(
    util_k_marginals(eval_data = ed, k = 1, group_by = "g")$score,
    750
  )
})

test_that("missing group values follow na.rm", {
  conf_gna <- tibble::tibble(
    g = c("A", "A", NA, NA),
    a = c("x", "y", "x", "y")
  )

  synth_gna <- tibble::tibble(
    g = c("A", "A", NA, NA),
    a = c("x", "x", "x", "y")
  )

  # na.rm = FALSE: NA forms its own stratum (perfect match -> 1000);
  # stratum A scores 500 -> overall 750
  kept <- suppressMessages(
    .util_k_marginals(
      synth_data = synth_gna, conf_data = conf_gna, k = 1, group_by = "g"
    )
  )

  expect_equal(kept$score, 750)
  expect_true("NA" %in% kept$group_scores$g)

  # na.rm = TRUE: NA-group rows drop entirely, leaving only stratum A
  dropped <- .util_k_marginals(
    synth_data = synth_gna, conf_data = conf_gna, k = 1, group_by = "g",
    na.rm = TRUE
  )

  expect_equal(dropped$score, 500)
  expect_equal(nrow(dropped$group_scores), 1)
})

test_that("multi-column group_by stratifies by joint combinations", {
  # four joint strata of two rows each; only stratum (A, p) diverges:
  #   conf (x = 0.5, y = 0.5), synth (x = 1) -> MabsDD = 0.5 -> score 500
  # the other three strata are identical -> 1000
  # overall = 0.25 * 500 + 0.75 * 1000 = 875
  conf_g2 <- tibble::tibble(
    g1 = c("A", "A", "A", "A", "B", "B", "B", "B"),
    g2 = c("p", "p", "q", "q", "p", "p", "q", "q"),
    a = c("x", "y", "x", "y", "x", "y", "x", "y")
  )

  synth_g2 <- dplyr::mutate(
    conf_g2,
    a = c("x", "x", "x", "y", "x", "y", "x", "y")
  )

  result <- .util_k_marginals(
    synth_data = synth_g2, conf_data = conf_g2, k = 1,
    group_by = c("g1", "g2")
  )

  expect_equal(result$score, 875)

  # both grouping columns ride along in every output
  expect_named(result$group_scores, c("g1", "g2", "share", "score"))
  expect_true(all(c("g1", "g2") %in% names(result$marginals)))
  expect_true(all(c("g1", "g2") %in% names(result$cells)))

  expect_equal(result$group_scores$share, rep(0.25, 4))

  # worst joint stratum first
  expect_equal(result$group_scores$g1[1], "A")
  expect_equal(result$group_scores$g2[1], "p")

  # neither grouping variable enters the marginal universe
  expect_false(any(result$marginals$variables %in% c("g1", "g2")))
})

test_that("partially missing joint strata follow na.rm", {
  # g2 is missing on rows 3-4; only the (A, NA) stratum diverges:
  #   conf (x = 0.5, y = 0.5), synth (x = 1) -> score 500
  conf_gpart <- tibble::tibble(
    g1 = c("A", "A", "A", "A"),
    g2 = c("p", "p", NA, NA),
    a = c("x", "y", "x", "y")
  )

  synth_gpart <- dplyr::mutate(conf_gpart, a = c("x", "y", "x", "x"))

  # na.rm = FALSE: the partial combination becomes an (A, "NA") stratum
  # overall = 0.5 * 1000 + 0.5 * 500 = 750
  kept <- suppressMessages(
    .util_k_marginals(
      synth_data = synth_gpart, conf_data = conf_gpart, k = 1,
      group_by = c("g1", "g2")
    )
  )

  expect_equal(kept$score, 750)
  expect_true("NA" %in% kept$group_scores$g2)

  # na.rm = TRUE: rows missing any grouping value drop, leaving (A, p) only
  dropped <- .util_k_marginals(
    synth_data = synth_gpart, conf_data = conf_gpart, k = 1,
    group_by = c("g1", "g2"), na.rm = TRUE
  )

  expect_equal(dropped$score, 1000)
  expect_equal(nrow(dropped$group_scores), 1)
})

test_that("empty, missing, and duplicate group_by values error", {
  expect_error(
    .util_k_marginals(
      synth_data = synth_g, conf_data = conf_g, k = 1,
      group_by = character(0)
    ),
    regexp = "`group_by` must be a character vector"
  )

  expect_error(
    .util_k_marginals(
      synth_data = synth_g, conf_data = conf_g, k = 1,
      group_by = c("g", NA)
    ),
    regexp = "`group_by` must be a character vector"
  )

  expect_error(
    .util_k_marginals(
      synth_data = synth_g, conf_data = conf_g, k = 1,
      group_by = c("g", "g")
    ),
    regexp = "must not contain duplicate"
  )
})

test_that("group_by composes with synth_varnames", {
  # b is shared but unsynthesized; g stratifies; the universe is {a} only
  conf_gsv <- tibble::tibble(
    g = c("A", "A", "B", "B"),
    a = c("x", "y", "x", "y"),
    b = c("m", "m", "n", "n")
  )

  synth_gsv <- dplyr::mutate(conf_gsv, a = c("x", "x", "x", "y"))

  result <- .util_k_marginals(
    synth_data = synth_gsv, conf_data = conf_gsv, k = 1,
    group_by = "g", synth_varnames = "a"
  )

  expect_equal(result$score, 750)
  expect_equal(unique(result$marginals$variables), "a")

  # a grouping variable named in synth_varnames still stratifies rather
  # than marginalizing
  result_gname <- .util_k_marginals(
    synth_data = synth_gsv, conf_data = conf_gsv, k = 1,
    group_by = "g", synth_varnames = c("g", "a")
  )

  expect_equal(unique(result_gname$marginals$variables), "a")
})

test_that("literal 'NA' strings in grouping columns collide with true NA", {
  # single grouping column carrying both a literal "NA" level and a true NA
  conf_collide <- tibble::tibble(
    g = c("NA", "A", NA),
    a = c("x", "x", "x")
  )

  synth_ok <- tibble::tibble(
    g = c("A", "A", "A"),
    a = c("x", "x", "x")
  )

  expect_error(
    suppressMessages(
      .util_k_marginals(
        synth_data = synth_ok, conf_data = conf_collide, k = 1,
        group_by = "g"
      )
    ),
    regexp = "'NA' already exists"
  )

  # multi-column grouping where only one key collides
  conf_multi <- tibble::tibble(
    g1 = c("A", "A", "A"),
    g2 = c("NA", "p", NA),
    a = c("x", "x", "x")
  )

  synth_multi <- tibble::tibble(
    g1 = c("A", "A", "A"),
    g2 = c("p", "p", "p"),
    a = c("x", "x", "x")
  )

  expect_error(
    suppressMessages(
      .util_k_marginals(
        synth_data = synth_multi, conf_data = conf_multi, k = 1,
        group_by = c("g1", "g2")
      )
    ),
    regexp = "'NA' already exists"
  )

  # convert_na_to_level() rejects a literal "NA" level even without true
  # missing values, so such data must be scored with na.rm = TRUE
  conf_legit <- tibble::tibble(
    g = c("NA", "NA", "A", "A"),
    a = c("x", "y", "x", "y")
  )

  synth_legit <- dplyr::mutate(conf_legit, a = c("x", "x", "x", "y"))

  expect_error(
    .util_k_marginals(
      synth_data = synth_legit, conf_data = conf_legit, k = 1, group_by = "g"
    ),
    regexp = "'NA' already exists"
  )

  # na.rm = TRUE bypasses the conversion, so the "NA" stratum scores
  # normally: stratum "NA" diverges (500), stratum A matches (1000)
  result <- .util_k_marginals(
    synth_data = synth_legit, conf_data = conf_legit, k = 1, group_by = "g",
    na.rm = TRUE
  )

  expect_equal(result$score, 750)
})

test_that("grouped results map over replicates with the same structure", {
  ed <- eval_data(
    conf_data = conf_g,
    synth_data = list(synth_g, conf_g)
  )

  result <- util_k_marginals(
    eval_data = ed, k = 1, group_by = "g"
  )

  expect_length(result, 2)

  for (rep in result) {
    expect_s3_class(rep, "k_marginals")
    expect_named(rep$group_scores, c("g", "share", "score"))
    expect_true("g" %in% names(rep$marginals))
    expect_true("g" %in% names(rep$cells))
  }

  # first replicate diverges in stratum A, second is identical data
  expect_equal(purrr::map_dbl(result, "score"), c(750, 1000))
})

test_that("a confidential stratum emptied by na.rm errors instead of NaN", {
  # stratum B's confidential rows are all missing on a, so per-marginal NA
  # removal leaves nothing to score against
  conf_gna2 <- tibble::tibble(
    g = c("A", "A", "B", "B"),
    a = c("x", "y", NA, NA)
  )

  synth_gna2 <- tibble::tibble(
    g = c("A", "A", "B", "B"),
    a = c("x", "y", "x", "y")
  )

  expect_error(
    .util_k_marginals(
      synth_data = synth_gna2, conf_data = conf_gna2, k = 1, group_by = "g",
      na.rm = TRUE
    ),
    regexp = "no rows remain"
  )

  # the synthetic side emptying in a stratum is still allowed
  conf_swap <- synth_gna2
  synth_swap <- conf_gna2

  result <- .util_k_marginals(
    synth_data = synth_swap, conf_data = conf_swap, k = 1, group_by = "g",
    na.rm = TRUE
  )

  # stratum B scores conf (x = 0.5, y = 0.5) against zero synth -> 500
  expect_equal(result$score, 750)
})

test_that("priority_vars with NA entries hits the intended error", {
  # %in% never propagates NA, so the membership test is FALSE, not NA, and
  # the package error fires rather than a base R condition failure
  expect_error(
    .util_k_marginals(
      synth_data = synth, conf_data = conf, k = 1,
      priority_vars = NA_character_
    ),
    regexp = "`priority_vars` must be a character vector of variables available"
  )

  expect_error(
    .util_k_marginals(
      synth_data = synth, conf_data = conf, k = 1,
      priority_vars = c("a", NA)
    ),
    regexp = "`priority_vars` must be a character vector of variables available"
  )
})

test_that("priority_vars = character(0) behaves like NULL", {
  # an empty priority set passes validation and simply guarantees nothing
  result <- .util_k_marginals(
    synth_data = synth, conf_data = conf, k = 1,
    priority_vars = character(0)
  )

  expect_equal(
    result$score,
    .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)$score
  )
})
