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

make_expected_cov_long <- function(conf_df, syn_df, use = "everything", method = "pearson") {
  stats::cov(
    x = conf_df |> dplyr::select(tidyselect::where(is.numeric)),
    y = syn_df |> dplyr::select(tidyselect::where(is.numeric)),
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
    dplyr::arrange(dplyr::across(dplyr::all_of(c("var1", "var2")))) -> out

  out[out$var1 > out$var2, , drop = FALSE]
}

test_that("util_cov returns a true lower-triangle long covariance table", {

  ed <- eval_data(conf_data = conf, synth_data = syn)

  cov_out <- util_cov(ed, method = "pearson")

  actual <- cov_out |>
    dplyr::select(var1, var2, covariance) |>
    dplyr::arrange(var1, var2)

  expected <- make_expected_cov_long(conf, syn, method = "pearson") |>
    dplyr::select(var1, var2, covariance) |>
    dplyr::arrange(var1, var2)

  expect_equal(actual, expected)
  expect_true(!any(actual$var1 == actual$var2))
  expect_equal(nrow(actual), 3)
})

test_that("util_cov works with group_by_q and labels groups", {

  conf_grouped <- conf |>
    dplyr::mutate(group = c("A", "A", "B", "B", "B"))

  syn_grouped <- syn |>
    dplyr::mutate(group = c("A", "A", "B", "B", "B"))

  ed_grouped <- eval_data(conf_data = conf_grouped, synth_data = syn_grouped)

  cov_grouped <- util_cov(ed_grouped, group_by_q = "group", method = "pearson")

  expect_true("group" %in% names(cov_grouped))
  expect_true(!any(cov_grouped$var1 == cov_grouped$var2))

  expected_grouped <- purrr::map_dfr(
    c("A", "B"),
    \(g) {
      make_expected_cov_long(
        conf_grouped |> dplyr::filter(group == g),
        syn_grouped |> dplyr::filter(group == g),
        method = "pearson"
      ) |>
        dplyr::mutate(group = g)
    }
  ) |>
    dplyr::select(group, var1, var2, covariance) |>
    dplyr::arrange(group, var1, var2)

  actual_grouped <- cov_grouped |>
    dplyr::select(group, var1, var2, covariance) |>
    dplyr::arrange(group, var1, var2)

  expect_equal(actual_grouped, expected_grouped)
})

test_that("util_cov supports non-default method", {

  conf_nl <- data.frame(
    a = 1:6,
    b = c(1, 4, 9, 16, 25, 36),
    c = c(2, 3, 5, 7, 11, 13)
  )

  ed_nl <- eval_data(conf_data = conf_nl, synth_data = conf_nl)

  cov_pearson <- util_cov(ed_nl, method = "pearson") |>
    dplyr::arrange(var1, var2)

  cov_spearman <- util_cov(ed_nl, method = "spearman") |>
    dplyr::arrange(var1, var2)

  expect_false(isTRUE(all.equal(cov_pearson$covariance, cov_spearman$covariance)))
})