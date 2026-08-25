# ---- fixtures and worked examples -------------------------------------------
#
# Every fixture below is defined once and reused throughout. Expected values
# in the tests are derived from the hand calculations here.
#
# conf / synth (two categorical variables)
#
#   conf               synth
#     a  b               a  b
#     x  p               x  p
#     x  p               y  q
#     y  p               y  q
#     y  q               y  p
#
#   k = 1
#     marginal a: conf (x = 0.50, y = 0.50), synth (x = 0.25, y = 0.75)
#       MabsDD = mean(0.25, 0.25) = 0.25
#     marginal b: conf (p = 0.75, q = 0.25), synth (p = 0.50, q = 0.50)
#       MabsDD = mean(0.25, 0.25) = 0.25
#     score = mean(0.25, 0.25) = 0.25
#
#   k = 2 (single combination a x b)
#     cells: conf (x,p = 0.50, y,p = 0.25, y,q = 0.25)
#            synth (x,p = 0.25, y,p = 0.25, y,q = 0.50)
#     per-cell |synth - conf|: (x,p) 0.25, (y,p) 0, (y,q) 0.25
#     MabsDD = mean(0.25, 0, 0.25) = 1/6 -> score 1/6
#
# conf_3 / synth_3 (conf / synth plus a third variable c)
#
#   conf_3             synth_3
#     a  b  c            a  b  c
#     x  p  m            x  p  m
#     x  p  m            y  q  n
#     y  p  n            y  q  n
#     y  q  n            y  p  n
#
#   k = 2
#     (a, b): as above, MabsDD = 1/6
#     (a, c): conf (x,m = 0.50, y,n = 0.50), synth (x,m = 0.25, y,n = 0.75)
#       MabsDD = mean(0.25, 0.25) = 1/4
#     (b, c): conf (p,m = 0.50, p,n = 0.25, q,n = 0.25)
#             synth (p,m = 0.25, p,n = 0.25, q,n = 0.50)
#       MabsDD = mean(0.25, 0, 0.25) = 1/6
#     worst first: (a, c) = 1/4, (a, b) = 1/6, (b, c) = 1/6
#
# conf_num / synth_num (one numeric variable)
#
#   conf_num           synth_num
#     v                  v
#     1                  1
#     2                  1
#     3                  1
#     4                  4
#
#   bins = 2 (width): cut at 2.5, so low = {1, 2}, high = {3, 4}
#     conf (low = 0.50, high = 0.50), synth (low = 0.75, high = 0.25)
#     MabsDD = mean(0.25, 0.25) = 0.25 -> score 0.25
#
# conf_na / synth_na (conf / synth with the last value of a missing)
#
#   conf_na            synth_na
#     a   b              a   b
#     x   p              x   p
#     x   p              y   q
#     y   p              y   q
#     NA  q              NA  p
#
#   na.rm = FALSE, k = 1 (NA is its own level)
#     marginal a: conf (x = 0.50, y = 0.25, NA = 0.25),
#                 synth (x = 0.25, y = 0.50, NA = 0.25)
#       MabsDD = mean(0.25, 0.25, 0) = 1/6
#     marginal b: MabsDD = 0.25
#     score = mean(1/6, 1/4) = 5/24
#   na.rm = TRUE, k = 1 (rows dropped per marginal)
#     marginal a (3 rows each): conf (x = 2/3, y = 1/3),
#                               synth (x = 1/3, y = 2/3)
#       MabsDD = 1/3
#     marginal b (all 4 rows): MabsDD = 0.25
#     score = mean(1/3, 1/4) = 7/24
#
# conf_g / synth_g (one grouping variable g, one marginal variable a)
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
  a = c("x", "x", "y", "y"),
  b = c("p", "p", "p", "q")
)

synth <- tibble::tibble(
  a = c("x", "y", "y", "y"),
  b = c("p", "q", "q", "p")
)

conf_3 <- dplyr::mutate(conf, c = c("m", "m", "n", "n"))
synth_3 <- dplyr::mutate(synth, c = c("m", "n", "n", "n"))

conf_num <- tibble::tibble(v = c(1, 2, 3, 4))
synth_num <- tibble::tibble(v = c(1, 1, 1, 4))

conf_na <- dplyr::mutate(conf, a = c("x", "x", "y", NA))
synth_na <- dplyr::mutate(synth, a = c("x", "y", "y", NA))

conf_g <- tibble::tibble(
  g = c("A", "A", "B", "B"),
  a = c("x", "y", "x", "y")
)

synth_g <- dplyr::mutate(conf_g, a = c("x", "x", "x", "y"))

# ---- core metric ------------------------------------------------------------

test_that("scores match hand-computed values", {
  expect_equal(
    .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)$score,
    0.25
  )

  expect_equal(
    .util_k_marginals(synth_data = synth, conf_data = conf, k = 2)$score,
    1 / 6
  )

  # identical data is a perfect match
  for (k in 1:2) {
    expect_equal(
      .util_k_marginals(synth_data = conf, conf_data = conf, k = k)$score,
      0
    )
  }
})

test_that("marginals and cells report worst-first detail", {
  result <- .util_k_marginals(synth_data = synth_3, conf_data = conf_3, k = 2)

  expect_s3_class(result, "k_marginals")
  expect_named(result, c("score", "marginals", "cells"))

  # combinations sorted by descending MabsDD
  expect_equal(result$marginals$variables, c("a, c", "a, b", "b, c"))
  expect_equal(result$marginals$madd, c(1 / 4, 1 / 6, 1 / 6))

  # cells sorted by descending absolute difference across all combinations
  expect_named(
    result$cells,
    c("variables", "cell", "prop_synth", "prop_conf", "abs_diff", "diff")
  )
  expect_equal(
    result$cells$abs_diff,
    sort(result$cells$abs_diff, decreasing = TRUE)
  )
})

test_that("cells absent from one dataset count as proportion zero", {
  # conf has level y that synth lacks
  # marginal a: conf (x = 0.5, y = 0.5), synth (x = 1, y = 0) -> score 0.5
  synth_x_only <- tibble::tibble(a = c("x", "x"))
  conf_xy <- tibble::tibble(a = c("x", "y"))

  result <- .util_k_marginals(
    synth_data = synth_x_only, conf_data = conf_xy, k = 1
  )

  expect_equal(result$score, 0.5)

  y_cell <- dplyr::filter(result$cells, .data$cell == "y")

  expect_equal(y_cell$prop_synth, 0)
  expect_equal(y_cell$prop_conf, 0.5)
})

test_that("variables not shared by both datasets are ignored", {
  base <- .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)

  expect_equal(
    .util_k_marginals(synth_data = synth_3, conf_data = conf, k = 1)$score,
    base$score
  )

  expect_equal(
    .util_k_marginals(synth_data = synth, conf_data = conf_3, k = 1)$score,
    base$score
  )
})

test_that("one-row datasets produce a valid result", {
  one_row <- tibble::tibble(a = "x", b = "p")

  result <- .util_k_marginals(synth_data = one_row, conf_data = one_row, k = 1)

  expect_equal(result$score, 0)
  expect_equal(nrow(result$cells), 2)
})

# ---- output and retention ---------------------------------------------------

test_that("keep_marginals and keep_cells retain the worst rows", {
  full <- .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)

  kept <- .util_k_marginals(
    synth_data = synth,
    conf_data = conf,
    k = 1,
    keep_marginals = 1,
    keep_cells = 2
  )

  expect_equal(kept$marginals, full$marginals[1, ])
  expect_equal(kept$cells, full$cells[1:2, ])

  # score is computed from all marginals before truncation
  expect_equal(kept$score, full$score)
})

test_that("print method reports the score and worst marginals", {
  result <- .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)

  expect_output(print(result), regexp = "k-marginals score: 0.25")
  expect_output(print(result), regexp = "Worst marginals:")

  # n truncates the display: row 1 prints, row 2 does not
  out <- utils::capture.output(print(result, n = 1))

  expect_true(any(grepl("^1 ", out)))
  expect_false(any(grepl("^2 ", out)))
})

# ---- n_marginals and priority_vars ------------------------------------------

test_that("n_marginals caps the evaluated combinations reproducibly", {
  set.seed(1)
  first <- .util_k_marginals(
    synth_data = synth_3,
    conf_data = conf_3,
    k = 2,
    n_marginals = 2
  )

  set.seed(1)
  second <- .util_k_marginals(
    synth_data = synth_3,
    conf_data = conf_3,
    k = 2,
    n_marginals = 2
  )

  expect_equal(nrow(first$marginals), 2)
  expect_equal(first, second)
})

test_that("n_marginals at or above the combination count changes nothing", {
  full <- .util_k_marginals(synth_data = synth_3, conf_data = conf_3, k = 2)

  capped <- .util_k_marginals(
    synth_data = synth_3,
    conf_data = conf_3,
    k = 2,
    n_marginals = 3
  )

  expect_equal(capped, full)
})

test_that("priority_vars combinations are always evaluated", {
  # the two pairs containing a fill the cap exactly
  result <- .util_k_marginals(
    synth_data = synth_3,
    conf_data = conf_3,
    k = 2,
    n_marginals = 2,
    priority_vars = "a"
  )

  expect_setequal(result$marginals$variables, c("a, b", "a, c"))
})

# ---- weight_var -------------------------------------------------------------

test_that("weighted proportions match hand-computed values", {
  # conf weight shares x = 3/4, y = 1/4; synth x = 1/2, y = 1/2
  # MabsDD = mean(0.25, 0.25) = 0.25
  conf_w <- tibble::tibble(a = c("x", "y"), w = c(3, 1))
  synth_w <- tibble::tibble(a = c("x", "y"), w = c(1, 1))

  result <- .util_k_marginals(
    synth_data = synth_w,
    conf_data = conf_w,
    k = 1,
    weight_var = "w"
  )

  expect_equal(result$score, 0.25)
  expect_equal(
    dplyr::filter(result$cells, .data$cell == "x")$prop_conf,
    0.75
  )
})

test_that("unit weights reproduce the unweighted result", {
  conf_w <- dplyr::mutate(conf, w = 1)
  synth_w <- dplyr::mutate(synth, w = 1)

  weighted <- .util_k_marginals(
    synth_data = synth_w,
    conf_data = conf_w,
    k = 1,
    weight_var = "w"
  )

  unweighted <- .util_k_marginals(synth_data = synth, conf_data = conf, k = 1)

  expect_equal(weighted, unweighted)

  # the weight column is never a marginal
  expect_setequal(weighted$marginals$variables, c("a", "b"))
})

# ---- group_by ---------------------------------------------------------------

test_that("group_by stratifies the score by confidential shares", {
  result <- .util_k_marginals(
    synth_data = synth_g,
    conf_data = conf_g,
    k = 1,
    group_by = "g"
  )

  expect_equal(result$score, 0.25)

  # group_scores are worst first
  expect_named(result$group_scores, c("g", "share", "score"))
  expect_equal(result$group_scores$g, c("A", "B"))
  expect_equal(result$group_scores$share, c(0.5, 0.5))
  expect_equal(result$group_scores$score, c(0.5, 0))

  # detail tables carry the grouping column, and g is never a marginal
  expect_true("g" %in% names(result$marginals))
  expect_true("g" %in% names(result$cells))
  expect_equal(result$marginals$g[1], "A")
  expect_false("g" %in% result$marginals$variables)

  expect_output(print(result), regexp = "Worst groups:")
})

test_that("ungrouped results carry no group_scores", {
  result <- .util_k_marginals(synth_data = synth_g, conf_data = conf_g, k = 1)

  expect_null(result$group_scores)
})

test_that("an empty synthetic stratum scores against zero proportions", {
  # synth has no B rows: stratum B conf (x = 0.5, y = 0.5) against 0 -> 0.5
  # stratum A identical -> 0; overall = 0.5 * 0 + 0.5 * 0.5 = 0.25
  synth_a_only <- dplyr::filter(conf_g, .data$g == "A")

  result <- .util_k_marginals(
    synth_data = synth_a_only,
    conf_data = conf_g,
    k = 1,
    group_by = "g"
  )

  expect_equal(result$score, 0.25)
})

# ---- synth_varnames ---------------------------------------------------------

test_that("synth_varnames restricts the marginal variables", {
  result <- .util_k_marginals(
    synth_data = synth_3,
    conf_data = conf_3,
    k = 2,
    synth_varnames = c("a", "b")
  )

  expect_equal(result$marginals$variables, "a, b")
})

test_that("synth_vars selects synthesized variables for postsynth input", {
  ed <- eval_data(conf_data = penguins_conf, synth_data = penguins_postsynth)

  synth_only <- util_k_marginals(eval_data = ed, k = 1, synth_vars = TRUE)
  all_shared <- util_k_marginals(eval_data = ed, k = 1, synth_vars = FALSE)

  expect_setequal(synth_only$marginals$variables, ed$synth_vars)
  expect_setequal(
    all_shared$marginals$variables,
    intersect(names(ed$conf_data), names(ed$synth_data))
  )

  # the carried-over variables differ between datasets, so the flag must
  # change the score
  expect_false(isTRUE(all.equal(synth_only$score, all_shared$score)))
})

test_that("synth_vars = TRUE is a no-op for plain data frame input", {
  ed <- eval_data(conf_data = conf, synth_data = synth)

  result <- util_k_marginals(eval_data = ed, k = 1, synth_vars = TRUE)

  expect_setequal(result$marginals$variables, c("a", "b"))
})

# ---- na.rm ------------------------------------------------------------------

test_that("NA values form their own level by default", {
  expect_message(
    result <- .util_k_marginals(
      synth_data = synth_na, conf_data = conf_na, k = 1
    ),
    regexp = "contain missing data: a"
  )

  expect_equal(result$score, 5 / 24)
  expect_true("NA" %in% result$cells$cell)
})

test_that("na.rm = TRUE drops missing values per marginal", {
  expect_no_message(
    result <- .util_k_marginals(
      synth_data = synth_na,
      conf_data = conf_na,
      k = 1,
      na.rm = TRUE
    )
  )

  expect_equal(result$score, 7 / 24)
  expect_false("NA" %in% result$cells$cell)
})

test_that("na.rm = TRUE drops rows per combination, not globally", {
  # a's NA sits on a different row in each dataset; b and c are identical
  #
  # pairwise deletion, k = 2:
  #   (b, c): all 4 rows, identical -> MabsDD = 0
  #   (a, b): conf drops row 4 -> (x,p), (x,q), (y,p) each 1/3
  #           synth drops row 3 -> (x,p), (x,q), (y,q) each 1/3
  #           MabsDD = mean(0, 0, 1/3, 1/3) = 1/6
  #   (a, c): conf (x,m) = 2/3, (y,n) = 1/3; synth identical -> 0
  #   score = mean(0, 1/6, 0) = 1/18
  #
  # listwise deletion would drop different rows from each dataset and
  # corrupt the complete (b, c) marginal, so madd = 0 there is the
  # discriminating assertion
  conf_pair <- tibble::tibble(
    a = c("x", "x", "y", NA),
    b = c("p", "q", "p", "q"),
    c = c("m", "m", "n", "n")
  )

  synth_pair <- dplyr::mutate(conf_pair, a = c("x", "x", NA, "y"))

  result <- .util_k_marginals(
    synth_data = synth_pair,
    conf_data = conf_pair,
    k = 2,
    na.rm = TRUE
  )

  expect_equal(result$score, 1 / 18)
  expect_equal(
    dplyr::filter(result$marginals, .data$variables == "b, c")$madd,
    0
  )
})

test_that("a literal 'NA' level alongside true NA values errors", {
  conf_lit <- tibble::tibble(a = c("NA", "x", NA))
  synth_ok <- tibble::tibble(a = c("x", "x", "x"))

  # in a marginal variable
  expect_error(
    suppressMessages(
      .util_k_marginals(synth_data = synth_ok, conf_data = conf_lit, k = 1)
    )
  )

  # in a grouping variable
  conf_lit_g <- dplyr::mutate(conf_lit, g = c("NA", "A", NA), a = "x")
  synth_ok_g <- dplyr::mutate(synth_ok, g = "A")

  expect_error(
    suppressMessages(
      .util_k_marginals(
        synth_data = synth_ok_g,
        conf_data = conf_lit_g,
        k = 1,
        group_by = "g"
      )
    )
  )
})

# ---- bins and discretize_method ---------------------------------------------

test_that("width discretization matches hand-computed values", {
  result <- .util_k_marginals(
    synth_data = synth_num,
    conf_data = conf_num,
    k = 1,
    bins = 2
  )

  expect_equal(result$score, 0.25)
  expect_equal(nrow(result$cells), 2)
})

test_that("ntile discretization uses confidential quantiles", {
  # quartiles of 1:8 give 4 bins of 2 values: conf shares 0.25 apiece
  # synth all in the lowest bin: shares (1, 0, 0, 0)
  # MabsDD = mean(0.75, 0.25, 0.25, 0.25) = 0.375
  conf_8 <- tibble::tibble(v = 1:8)
  synth_8 <- tibble::tibble(v = rep(1, 8))

  result <- .util_k_marginals(
    synth_data = synth_8,
    conf_data = conf_8,
    k = 1,
    bins = 4,
    discretize_method = "ntile"
  )

  expect_equal(result$score, 0.375)
})

test_that("cluster discretization separates well-separated groups", {
  # clusters around 1 and 10: conf shares (0.5, 0.5), synth (1, 0) -> 0.5
  conf_cl <- tibble::tibble(v = c(1, 1.1, 10, 10.1))
  synth_cl <- tibble::tibble(v = c(1, 1, 1.1, 1.1))

  set.seed(20250813)

  result <- .util_k_marginals(
    synth_data = synth_cl,
    conf_data = conf_cl,
    k = 1,
    bins = 2,
    discretize_method = "cluster"
  )

  expect_equal(result$score, 0.5)
})

test_that("synthetic values outside the confidential range land in edge bins", {
  synth_wide <- tibble::tibble(v = c(-100, -100, 100, 100))

  result <- .util_k_marginals(
    synth_data = synth_wide,
    conf_data = conf_num,
    k = 1,
    bins = 2
  )

  # extremes split evenly across the two edge bins, matching conf shares
  expect_equal(result$score, 0)
})

test_that("bins = NULL leaves numeric variables as-is", {
  result <- .util_k_marginals(
    synth_data = synth_num,
    conf_data = conf_num,
    k = 1
  )

  # every distinct value is its own cell
  expect_equal(nrow(result$cells), 4)
})

# ---- interactions -----------------------------------------------------------

test_that("missing group values follow na.rm", {
  conf_gna <- dplyr::mutate(conf_g, g = c("A", "A", NA, NA))
  synth_gna <- dplyr::mutate(synth_g, g = c("A", "A", NA, NA))

  # na.rm = FALSE: NA is its own stratum (perfect match -> 0);
  # stratum A scores 0.5 -> overall 0.25
  kept <- suppressMessages(
    .util_k_marginals(
      synth_data = synth_gna,
      conf_data = conf_gna,
      k = 1,
      group_by = "g"
    )
  )

  expect_equal(kept$score, 0.25)
  expect_true("NA" %in% kept$group_scores$g)

  # na.rm = TRUE: NA-group rows drop entirely, leaving only stratum A
  dropped <- .util_k_marginals(
    synth_data = synth_gna,
    conf_data = conf_gna,
    k = 1,
    group_by = "g",
    na.rm = TRUE
  )

  expect_equal(dropped$score, 0.5)
  expect_equal(nrow(dropped$group_scores), 1)
})

# ---- input errors -----------------------------------------------------------
#
# One representative bad value per rule. Exact messages for the
# data-dependent checks are pinned in test-prepare_k_marginals_inputs.R.

test_that(".util_k_marginals() input errors work", {
  conf_w <- dplyr::mutate(conf, w = 1)
  synth_w <- dplyr::mutate(synth, w = 1)

  # inputs must be non-empty data frames
  expect_error(.util_k_marginals(synth_data = 1, conf_data = conf, k = 1))
  expect_error(
    .util_k_marginals(synth_data = synth, conf_data = conf[0, ], k = 1)
  )

  # k must be a single integer in 1:3
  expect_error(.util_k_marginals(synth_data = synth, conf_data = conf, k = 4))
  expect_error(.util_k_marginals(synth_data = synth, conf_data = conf, k = "1"))

  # k cannot exceed the available variables
  expect_error(.util_k_marginals(synth_data = synth, conf_data = conf, k = 3))

  # retention and sampling caps must be single integers >= 1 or Inf
  expect_error(
    .util_k_marginals(
      synth_data = synth,
      conf_data = conf,
      k = 1,
      keep_marginals = 0
    )
  )
  expect_error(
    .util_k_marginals(
      synth_data = synth,
      conf_data = conf,
      k = 1,
      keep_cells = 1.5
    )
  )
  expect_error(
    .util_k_marginals(
      synth_data = synth,
      conf_data = conf,
      k = 1,
      n_marginals = NA
    )
  )

  # priority_vars must name available variables
  expect_error(
    .util_k_marginals(
      synth_data = synth,
      conf_data = conf,
      k = 1,
      priority_vars = "zzz"
    )
  )
  expect_error(
    .util_k_marginals(
      synth_data = synth_3,
      conf_data = conf_3,
      k = 1,
      priority_vars = "c",
      synth_varnames = c("a", "b")
    )
  )

  # weight_var must be a numeric column in both datasets with valid weights
  expect_error(
    .util_k_marginals(
      synth_data = synth,
      conf_data = conf_w,
      k = 1,
      weight_var = "w"
    )
  )
  expect_error(
    .util_k_marginals(
      synth_data = synth_w,
      conf_data = dplyr::mutate(conf, w = -1),
      k = 1,
      weight_var = "w"
    )
  )

  # group_by must name columns in both datasets and exclude weight_var
  expect_error(
    .util_k_marginals(
      synth_data = synth,
      conf_data = conf,
      k = 1,
      group_by = "zzz"
    )
  )
  expect_error(
    .util_k_marginals(
      synth_data = synth_w,
      conf_data = conf_w,
      k = 1,
      group_by = "w",
      weight_var = "w"
    )
  )

  # synth_varnames must be a non-empty character vector matching something
  expect_error(
    .util_k_marginals(
      synth_data = synth,
      conf_data = conf,
      k = 1,
      synth_varnames = 1
    )
  )
  expect_error(
    .util_k_marginals(
      synth_data = synth,
      conf_data = conf,
      k = 1,
      synth_varnames = "zzz"
    )
  )

  # na.rm must be a single TRUE or FALSE
  expect_error(
    .util_k_marginals(
      synth_data = synth,
      conf_data = conf,
      k = 1,
      na.rm = NA
    )
  )

  # bins must be a single integer >= 2 with a known method
  expect_error(
    .util_k_marginals(
      synth_data = synth_num,
      conf_data = conf_num,
      k = 1,
      bins = 1
    )
  )
  expect_error(
    .util_k_marginals(
      synth_data = synth_num,
      conf_data = conf_num,
      k = 1,
      bins = 2,
      discretize_method = "magic"
    )
  )
})

test_that("util_k_marginals() input errors work", {
  ed <- eval_data(conf_data = conf, synth_data = synth)

  # eval_data must be an eval_data object
  expect_error(util_k_marginals(eval_data = synth, k = 1))

  # synth_vars must be a single TRUE or FALSE
  expect_error(util_k_marginals(eval_data = ed, k = 1, synth_vars = NA))

  # eval_data recording no synthesized variables cannot restrict to them
  ed_empty <- eval_data(
    conf_data = conf,
    synth_data = synth,
    synth_vars = character(0)
  )

  expect_error(util_k_marginals(eval_data = ed_empty, k = 1, synth_vars = TRUE))
  expect_no_error(
    util_k_marginals(eval_data = ed_empty, k = 1, synth_vars = FALSE)
  )

  # k is bounded by the synthesized-variable set when restricted
  ed_a <- eval_data(conf_data = conf, synth_data = synth, synth_vars = "a")

  expect_error(util_k_marginals(eval_data = ed_a, k = 2, synth_vars = TRUE))
  expect_no_error(util_k_marginals(eval_data = ed_a, k = 2, synth_vars = FALSE))
})

# ---- util_k_marginals() wrapper ---------------------------------------------

test_that("util_k_marginals() forwards every argument to the helper", {
  conf_w <- dplyr::mutate(conf_na, c = conf_3$c, w = c(1, 2, 1, 1))
  synth_w <- dplyr::mutate(synth_na, c = synth_3$c, w = c(1, 1, 2, 1))

  ed <- eval_data(conf_data = conf_w, synth_data = synth_w)

  args <- list(
    k = 2,
    keep_marginals = 2,
    keep_cells = 3,
    n_marginals = Inf,
    priority_vars = "a",
    weight_var = "w",
    group_by = NULL,
    na.rm = TRUE
  )

  via_wrapper <- do.call(
    util_k_marginals,
    c(list(eval_data = ed, synth_vars = FALSE), args)
  )

  via_helper <- do.call(
    .util_k_marginals,
    c(
      list(synth_data = synth_w, conf_data = conf_w, synth_varnames = NULL),
      args
    )
  )

  expect_equal(via_wrapper, via_helper)
})

test_that("util_k_marginals() maps over replicates", {
  ed <- eval_data(conf_data = conf_g, synth_data = list(synth_g, conf_g))

  ungrouped <- util_k_marginals(eval_data = ed, k = 1)
  grouped <- util_k_marginals(eval_data = ed, k = 1, group_by = "g")

  expect_length(ungrouped, 2)
  expect_length(grouped, 2)

  for (rep in ungrouped) {
    expect_s3_class(rep, "k_marginals")
    expect_named(rep, c("score", "marginals", "cells"))
  }

  for (rep in grouped) {
    expect_s3_class(rep, "k_marginals")
    expect_named(rep$group_scores, c("g", "share", "score"))
  }

  # second replicate is identical data
  expect_equal(purrr::map_dbl(grouped, "score"), c(0.25, 0))
})

test_that("util_k_marginals() messages the resolved discretization method", {
  ed <- eval_data(conf_data = conf_num, synth_data = synth_num)

  expect_message(
    util_k_marginals(eval_data = ed, k = 1, bins = 2),
    regexp = "2 bins using the 'width' method"
  )

  expect_message(
    util_k_marginals(
      eval_data = ed,
      k = 1,
      bins = 2,
      discretize_method = "ntile"
    ),
    regexp = "2 bins using the 'ntile' method"
  )

  expect_no_message(util_k_marginals(eval_data = ed, k = 1))
})
