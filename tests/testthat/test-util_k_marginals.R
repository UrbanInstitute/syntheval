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
    regexp = "shared by both datasets"
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
    c("variables", "cell", "prop_synth", "prop_conf", "abs_diff")
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

test_that("priority_vars combinations are always evaluated", {

  conf_3 <- dplyr::mutate(conf, c = c("m", "m", "n", "n"))
  synth_3 <- dplyr::mutate(synth, c = c("m", "n", "n", "n"))

  # the two combinations containing a fill the cap exactly, so the
  # selection is deterministic despite sampling
  result <- .util_k_marginals(
    synth_data = synth_3,
    conf_data = conf_3,
    k = 2,
    n_marginals = 2,
    priority_vars = "a"
  )

  expect_equal(sort(result$marginals$variables), c("a, b", "a, c"))

})

test_that("priority combinations exceeding n_marginals are all kept", {

  conf_3 <- dplyr::mutate(conf, c = c("m", "m", "n", "n"))
  synth_3 <- dplyr::mutate(synth, c = c("m", "n", "n", "n"))

  result <- .util_k_marginals(
    synth_data = synth_3,
    conf_data = conf_3,
    k = 2,
    n_marginals = 1,
    priority_vars = "a"
  )

  expect_equal(sort(result$marginals$variables), c("a, b", "a, c"))

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
    regexp = "`priority_vars` must be a character vector"
  )

  expect_error(
    .util_k_marginals(
      synth_data = synth, conf_data = conf, k = 1, priority_vars = 1
    ),
    regexp = "`priority_vars` must be a character vector"
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

test_that("sampling fills remaining slots after priority combinations", {

  # 4 shared variables, k = 2: 6 combinations, 3 containing a
  conf_4 <- dplyr::mutate(
    conf, c = c("m", "m", "n", "n"), d = c("u", "v", "u", "v")
  )
  synth_4 <- dplyr::mutate(
    synth, c = c("m", "n", "n", "n"), d = c("v", "v", "u", "u")
  )

  set.seed(20250813)

  result <- .util_k_marginals(
    synth_data = synth_4,
    conf_data = conf_4,
    k = 2,
    n_marginals = 4,
    priority_vars = "a"
  )

  # cap respected exactly: all 3 priority combos plus 1 sampled non-priority
  expect_equal(nrow(result$marginals), 4)

  has_a <- grepl("a", result$marginals$variables)
  expect_equal(sum(has_a), 3)
  expect_equal(sum(!has_a), 1)
  expect_true(
    all(result$marginals$variables[!has_a] %in% c("b, c", "b, d", "c, d"))
  )

})

test_that("n_marginals caps k = 3 combinations", {

  # 4 shared variables, k = 3: 4 combinations
  conf_4 <- dplyr::mutate(
    conf, c = c("m", "m", "n", "n"), d = c("u", "v", "u", "v")
  )
  synth_4 <- dplyr::mutate(
    synth, c = c("m", "n", "n", "n"), d = c("v", "v", "u", "u")
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
    conf, c = c("m", "m", "n", "n"), d = c("u", "v", "u", "v")
  )
  synth_4 <- dplyr::mutate(
    synth, c = c("m", "n", "n", "n"), d = c("v", "v", "u", "u")
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
    c(1, 1, 1, -1),   # negative
    c(1, 1, 1, NA),   # missing
    c(1, 1, 1, Inf),  # non-finite
    c(0, 0, 0, 0)     # zero total
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

test_that("invalid discretization arguments throw an error", {

  conf_num <- tibble::tibble(v = c(1, 2, 3, 4))
  synth_num <- tibble::tibble(v = c(1, 1, 1, 4))

  for (bad_bins in list(1, 2.5, "2", NA_real_, c(2, 3))) {

    expect_error(
      .util_k_marginals(
        synth_data = synth_num, conf_data = conf_num, k = 1, bins = bad_bins
      ),
      regexp = "`bins` must be a single integer >= 2"
    )

  }

  expect_error(
    .util_k_marginals(
      synth_data = synth_num, conf_data = conf_num, k = 1, bins = 2,
      discretize_method = "magic"
    )
  )

  # non-finite confidential values cannot be discretized
  conf_inf <- tibble::tibble(v = c(1, 2, 3, Inf))

  expect_error(
    .util_k_marginals(
      synth_data = synth_num, conf_data = conf_inf, k = 1, bins = 2
    ),
    regexp = "must be finite to discretize"
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

test_that("too few distinct confidential values throw an error", {

  conf_const <- tibble::tibble(v = c(2, 2, 2, 2))
  synth_num <- tibble::tibble(v = c(1, 1, 1, 4))

  for (method in c("width", "ntile", "cluster")) {

    expect_error(
      .util_k_marginals(
        synth_data = synth_num, conf_data = conf_const, k = 1, bins = 2,
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
    result <- .util_k_marginals(
      synth_data = synth_num, conf_data = conf_ties, k = 1, bins = 4,
      discretize_method = "ntile"
    ),
    regexp = "bins instead of 4 because of tied cut points"
  )

  expect_lt(nrow(result$cells), 5)

})
