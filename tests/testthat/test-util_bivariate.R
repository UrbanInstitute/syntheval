# Covariance -------------------------------
# sample data- numeric
# confidential data
conf <- data.frame(
  a = c(1, 2, 3, 4, 5),
  b = c(1, 2, 3, 4, 5),
  c = c(1, 2, 3, 4, 5),
  RECID = c("a", "b", "c", "a", "c")
)

# synthetic data
syn <- data.frame(
  a = c(1.5, 2, 3, 3, 6),
  b = c(1, 2.5, 2.5, 4, 5),
  c = c(1, 1, 1, 3, 4),
  RECID = c("a", "b", "c", "a", "c")
)

make_expected_cov_long <- function(df, use = "everything", method = "pearson") {
  out <- stats::cov(
    x = df |> dplyr::select(tidyselect::where(is.numeric)),
    use = use,
    method = method
  ) |>
    as.data.frame() |>
    tibble::rownames_to_column("var1") |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of("var1"),
      names_to = "var2",
      values_to = "covariance"
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c("var1", "var2"))))

  out[out$var1 >= out$var2, , drop = FALSE]
}

test_that("util_bivariate returns a true lower-triangle long covariance table", {
  ed <- eval_data(conf_data = conf, synth_data = syn)
  cov_out <- util_bivariate(ed, statistic = "covariance", method = "pearson")

  actual <- cov_out$covariance_original |>
    dplyr::select(var1, var2, covariance) |>
    dplyr::arrange(var1, var2)

  expected <- make_expected_cov_long(conf, method = "pearson") |>
    dplyr::select(var1, var2, covariance) |>
    dplyr::arrange(var1, var2)

  expect_equal(actual, expected)
  expect_true(!any(actual$var1 < actual$var2))
  expect_equal(nrow(actual), 6)
})

testthat::test_that("util_bivariate covariance returns correct full output set", {
  ed <- eval_data(conf_data = conf, synth_data = syn)

  out <- util_bivariate(
    eval_data = ed,
    statistic = "covariance",
    use = "everything",
    method = "pearson"
  )

  # 1) names and classes
  testthat::expect_named(
    out,
    c(
      "covariance_original",
      "covariance_synthetic",
      "covariance_difference",
      "covariance_fit",
      "covariance_difference_mae",
      "covariance_difference_rmse"
    )
  )

  testthat::expect_s3_class(out$covariance_original, "data.frame")
  testthat::expect_s3_class(out$covariance_synthetic, "data.frame")
  testthat::expect_s3_class(out$covariance_difference, "data.frame")

  # 2) original/synthetic tables
  expected_original <- make_expected_cov_long(conf, method = "pearson") |>
    dplyr::select(var1, var2, covariance) |>
    dplyr::arrange(var1, var2)

  expected_synthetic <- make_expected_cov_long(syn, method = "pearson") |>
    dplyr::select(var1, var2, covariance) |>
    dplyr::arrange(var1, var2)

  actual_original <- out$covariance_original |>
    dplyr::select(var1, var2, covariance) |>
    dplyr::arrange(var1, var2)

  actual_synthetic <- out$covariance_synthetic |>
    dplyr::select(var1, var2, covariance) |>
    dplyr::arrange(var1, var2)

  testthat::expect_equal(actual_original, expected_original)
  testthat::expect_equal(actual_synthetic, expected_synthetic)

  # 3) difference table
  expected_difference <- dplyr::left_join(
    expected_original,
    expected_synthetic,
    by = c("var1", "var2"),
    suffix = c("_orig", "_syn")
  ) |>
    dplyr::mutate(difference = .data$covariance_syn - .data$covariance_orig) |>
    dplyr::select(var1, var2, difference) |>
    dplyr::arrange(var1, var2)

  actual_difference <- out$covariance_difference |>
    dplyr::select(var1, var2, difference) |>
    dplyr::arrange(var1, var2)

  testthat::expect_equal(actual_difference, expected_difference, tolerance = 1e-12)

  # 4) scalar metrics from difference
  d <- actual_difference$difference
  n <- sum(!is.na(d))
  sse <- sum(d^2, na.rm = TRUE)
  n_cells <- length(d)

  expected_fit <- if (n == 0) {
    NA_real_
  } else if (sse == 0) {
    0
  } else if (n_cells == 0) {
    NA_real_
  } else {
    sqrt(sse) / n_cells
  }

  expected_mae <- if (n == 0) NA_real_ else mean(abs(d), na.rm = TRUE)
  expected_rmse <- if (n == 0) NA_real_ else sqrt(mean(d^2, na.rm = TRUE))

  testthat::expect_equal(out$covariance_fit, expected_fit, tolerance = 1e-12)
  testthat::expect_equal(out$covariance_difference_mae, expected_mae, tolerance = 1e-12)
  testthat::expect_equal(out$covariance_difference_rmse, expected_rmse, tolerance = 1e-12)
})

test_that("util_bivariate works with group_by_q and labels groups", {
  conf_grouped <- conf |> dplyr::mutate(group = c("A", "A", "B", "B", "B"))
  syn_grouped  <- syn  |> dplyr::mutate(group = c("A", "A", "B", "B", "B"))
  ed_grouped <- eval_data(conf_data = conf_grouped, synth_data = syn_grouped)

  cov_grouped <- util_bivariate(
    ed_grouped,
    statistic = "covariance",
    group_by_q = "group",
    method = "pearson"
  )

  actual_grouped <- cov_grouped$covariance_original |>
    dplyr::select(group, var1, var2, covariance) |>
    dplyr::arrange(group, var1, var2)

  expected_grouped <- purrr::map_dfr(
    c("A", "B"),
    \(g) {
      make_expected_cov_long(
        conf_grouped |> dplyr::filter(group == g),
        method = "pearson"
      ) |>
        dplyr::mutate(group = g)
    }
  ) |>
    dplyr::select(group, var1, var2, covariance) |>
    dplyr::arrange(group, var1, var2)

  expect_equal(actual_grouped, expected_grouped)
})

test_that("util_bivariate (covariance) supports non-default method", {

  conf_nl <- data.frame(
    a = 1:6,
    b = c(1, 4, 9, 16, 25, 36),
    c = c(2, 3, 5, 7, 11, 13)
  )

  ed_nl <- eval_data(conf_data = conf_nl, synth_data = conf_nl)

  cov_pearson <- util_bivariate(ed_nl, statistic = "covariance", method = "pearson")$covariance_original |>
    dplyr::arrange(var1, var2)

  cov_spearman <- util_bivariate(ed_nl, statistic = "covariance", method = "spearman")$covariance_original |>
    dplyr::arrange(var1, var2)

  expect_false(isTRUE(all.equal(cov_pearson$covariance, cov_spearman$covariance)))
})

# RMI ----------------
# sample data - factor
set.seed(123)

  rmi_input <- data.frame(
    sex = factor(sample(c("F", "M"), 200, replace = TRUE)),
    race = factor(sample(c("White", "Black", "Asian", "Other"), 200, replace = TRUE)),
    region = factor(sample(c("Northeast", "Midwest", "South", "West"), 200, replace = TRUE)),
    insured = factor(sample(c("Yes", "No"), 200, replace = TRUE)),
    stringsAsFactors = TRUE
  )

  # optional missingness
  rmi_input$race[sample(seq_len(nrow(rmi_input)), 10)] <- NA
  rmi_input$insured[sample(seq_len(nrow(rmi_input)), 8)] <- NA

  set.seed(456)

  rmi_input_2 <- data.frame(
    sex = factor(
      sample(c("F", "M"), 200, replace = TRUE, prob = c(0.58, 0.42)),
      levels = levels(rmi_input$sex)
    ),
    race = factor(
      sample(c("White", "Black", "Asian", "Other"), 200, replace = TRUE,
             prob = c(0.45, 0.25, 0.20, 0.10)),
      levels = levels(rmi_input$race)
    ),
    region = factor(
      sample(c("Northeast", "Midwest", "South", "West"), 200, replace = TRUE,
             prob = c(0.18, 0.22, 0.38, 0.22)),
      levels = levels(rmi_input$region)
    ),
    insured = factor(
      sample(c("Yes", "No"), 200, replace = TRUE, prob = c(0.72, 0.28)),
      levels = levels(rmi_input$insured)
    ),
    stringsAsFactors = TRUE
  )

  rmi_input_2$race[sample(seq_len(nrow(rmi_input_2)), 14)] <- NA
  rmi_input_2$insured[sample(seq_len(nrow(rmi_input_2)), 5)] <- NA

testthat::test_that(".calc_rmi_tibble returns expected number of non-diagonal variable pairings", {

  # calculate number of non-diagonal pairings in rmi_input
  p <- ncol(rmi_input)
  n_expected <- p * (p - 1)

  # grouped
  out <- .calc_rmi_tibble(df = rmi_input, group_by_q = NULL)
  n_actual <- out |>
    nrow()

  n_diag <- out |>
    dplyr::filter(.data$var1 == .data$var2) |>
    nrow()

  testthat::expect_equal(n_actual, n_expected)
  testthat::expect_equal(n_diag, 0)

})

testthat::test_that(".util_bivariate computes ungrouped RMI outputs with expected structure", {

  out <- .util_bivariate(
    synth_data = rmi_input,
    conf_data = rmi_input_2,
    statistic = "rmi"
  )

  testthat::expect_named(
    out,
    c(
      "rmi_original",
      "rmi_synthetic",
      "rmi_difference",
      "rmi_fit",
      "rmi_difference_mae",
      "rmi_difference_rmse"
    )
  )

  testthat::expect_s3_class(out$rmi_original, "data.frame")
  testthat::expect_s3_class(out$rmi_synthetic, "data.frame")
  testthat::expect_s3_class(out$rmi_difference, "data.frame")

  testthat::expect_true(all(c("var1", "var2", "rmi") %in% names(out$rmi_original)))
  testthat::expect_true(all(c("var1", "var2", "rmi") %in% names(out$rmi_synthetic)))
  testthat::expect_true(all(c("var1", "var2", "difference") %in% names(out$rmi_difference)))

  testthat::expect_gt(nrow(out$rmi_original), 0)
  testthat::expect_equal(nrow(out$rmi_original), nrow(out$rmi_synthetic))
  testthat::expect_equal(nrow(out$rmi_original), nrow(out$rmi_difference))

  # difference consistency: synthetic - original
  chk <- dplyr::left_join(
    out$rmi_original,
    out$rmi_synthetic,
    by = c("var1", "var2"),
    suffix = c("_orig", "_syn")
  ) |>
    dplyr::mutate(calc_diff = .data$rmi_syn - .data$rmi_orig) |>
    dplyr::left_join(out$rmi_difference, by = c("var1", "var2"))

  testthat::expect_equal(chk$calc_diff, chk$difference, tolerance = 1e-12)

  testthat::expect_true(is.numeric(out$rmi_fit))
  testthat::expect_true(is.numeric(out$rmi_difference_mae))
  testthat::expect_true(is.numeric(out$rmi_difference_rmse))
})

testthat::test_that(".util_bivariate computes grouped RMI outputs with expected structure", {

  out <- .util_bivariate(
    synth_data = rmi_input,
    conf_data = rmi_input_2,
    statistic = "rmi",
    use = "pairwise.complete.obs",
    group_by_q = "insured"
  )

  testthat::expect_named(
    out,
    c(
      "rmi_original",
      "rmi_synthetic",
      "rmi_difference",
      "rmi_fit",
      "rmi_difference_mae",
      "rmi_difference_rmse"
    )
  )

  testthat::expect_s3_class(out$rmi_original, "data.frame")
  testthat::expect_s3_class(out$rmi_synthetic, "data.frame")
  testthat::expect_s3_class(out$rmi_difference, "data.frame")

  testthat::expect_true(all(c("insured", "var1", "var2", "rmi") %in% names(out$rmi_original)))
  testthat::expect_true(all(c("insured", "var1", "var2", "rmi") %in% names(out$rmi_synthetic)))
  testthat::expect_true(all(c("insured", "var1", "var2", "difference") %in% names(out$rmi_difference)))

  testthat::expect_gt(nrow(out$rmi_original), 0)
  testthat::expect_equal(nrow(out$rmi_original), nrow(out$rmi_synthetic))
  testthat::expect_equal(nrow(out$rmi_original), nrow(out$rmi_difference))

  # difference consistency: synthetic - original
  chk <- dplyr::left_join(
    out$rmi_original,
    out$rmi_synthetic,
    by = c("insured", "var1", "var2"),
    suffix = c("_orig", "_syn")
  ) |>
    dplyr::mutate(calc_diff = .data$rmi_syn - .data$rmi_orig) |>
    dplyr::left_join(out$rmi_difference, by = c("insured", "var1", "var2"))

  testthat::expect_equal(chk$calc_diff, chk$difference, tolerance = 1e-12)

  testthat::expect_true(is.numeric(out$rmi_fit$fit))
  testthat::expect_true(is.numeric(out$rmi_difference_mae$difference_mae))
  testthat::expect_true(is.numeric(out$rmi_difference_rmse$difference_rmse))
})
