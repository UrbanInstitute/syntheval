# confidential data
df <- data.frame(a = c(1, 2, 3),
                 b = c(1, 2, 3),
                 c = c(1, 2, 3),
                 RECID = c("a", "b", "c"))

# true lower triangle: diagonal elements removed and each pair appears only
# once (var1 > var2), so no duplicate/mirrored pairings remain
diff_table <- tibble::tibble(
  var1 = c("b", "b", "c"),
  var2 = c("a", "c", "a"),
  difference = c(0, -2, -2)
)


test_that("util_corr_fit returns a true lower triangle correlation matrix", {
  
  # create a synthetic data frame with the same structure as df
  synth_data <- data.frame(a = c(1, 2, 3),
                           b = c(1, 2, 3),
                           c = c(3, 2, 1),
                           RECID = c("a", "b", "c"))
  
  ed <- eval_data(conf_data = df, synth_data = synth_data)
  
  corr <- util_corr_fit(ed, method = "pearson")
  
  original_lt <- corr$correlation_original
  synthetic_lt <- corr$correlation_synthetic
  
  # check that the variable pairs in the original and synthetic data correlation matrices are the same
  original_pairs <- original_lt |>
    dplyr::distinct(var1, var2)
  
  synthetic_pairs <- synthetic_lt |>
    dplyr::distinct(var1, var2)
  
  expect_true(dplyr::setequal(original_pairs, synthetic_pairs))
  
  # check that the diagonal elements are removed
  expect_true(!any(original_lt$var1 == original_lt$var2))
  expect_true(!any(synthetic_lt$var1 == synthetic_lt$var2))

  # with 3 numeric variables (a, b, c) there are only 3 unique pairs in a true
  # lower triangle (no mirrored/duplicate pairings like a-b and b-a)
  expect_equal(nrow(original_lt), 3)
  expect_equal(nrow(synthetic_lt), 3)

  # now check pairs are distinct within groups if using group_by_q
  df_grouped <- df |>
    dplyr::mutate(group = c("A", "A", "B"))

  ed_grouped <- eval_data(conf_data = df_grouped, synth_data = df_grouped)

  corr_grouped <- util_corr_fit(ed_grouped, group_by_q = "group", method = "pearson")

  correlation_matrix <- corr_grouped$correlation_original

  expect_true(!any(correlation_matrix$var1 == correlation_matrix$var2))

  # check that the variable pairs are distinct within each group, and that
  # each group only has the 3 unique lower-triangle pairs (no duplicates)
  distinct_pairs <- correlation_matrix |>
    dplyr::group_by(dplyr::across(dplyr::all_of("group"))) |>
    dplyr::distinct(var1, var2)

  expect_equal(nrow(distinct_pairs), nrow(correlation_matrix))
  expect_true(all(correlation_matrix |>
    dplyr::count(group) |>
    dplyr::pull(n) == 3))
  
})

# test with postsynth, ungrouped
test_that("util_corr_fit is correct with postsynth object, ungrouped", {

  syn <- list(synthetic_data = data.frame(a = c(1, 2, 3),
                                          c = c(3, 2, 1),
                                          b = c(1, 2, 3),
                                          RECID = c("a", "b", "c")),
              jth_synthesis_time = data.frame(
                variable = factor(c("a", "c", "b"))
              )) |>
    structure(class = "postsynth")
  ed <- eval_data(conf_data = df, synth_data = syn)
  
  corr <- util_corr_fit(ed, method = "pearson")
  
  actual_diff <- corr$correlation_difference |>
    dplyr::select(var1, var2, difference) |>
    dplyr::arrange(var1, var2)

  n_nonzero_cells <- corr$correlation_difference |>
    dplyr::filter(.data$difference != 0) |>
    nrow()

  expected_diff <- diff_table |>
    dplyr::arrange(var1, var2)


  expect_equal(actual_diff, expected_diff)
  # Verify no diagonal elements
  expect_true(!any(actual_diff$var1 == actual_diff$var2))
  expect_equal(
    corr$correlation_fit,
    sqrt(sum(expected_diff$difference ^ 2)) / n_nonzero_cells
  )
  expect_equal(
    corr$correlation_difference_mae,
    mean(abs(expected_diff$difference))
  )
  expect_equal(
    corr$correlation_difference_rmse,
    sqrt(mean(expected_diff$difference ^ 2))
  )
  
})

# test with data
test_that("util_corr_fit is correct with eval_data object, ungrouped", {
  
  syn <- data.frame(a = c(1, 2, 3),
                    c = c(3, 2, 1),
                    b = c(1, 2, 3),
                    RECID = c("a", "b", "c"))
  
  ed <- eval_data(conf_data = df, synth_data = syn)
  
  corr <- util_corr_fit(ed, method = "pearson")

  actual_diff <- corr$correlation_difference |>
    dplyr::select(var1, var2, difference) |>
    dplyr::arrange(var1, var2)

  n_nonzero_cells <- corr$correlation_difference |>
    dplyr::filter(.data$difference != 0) |>
    nrow()

  expected_diff <- diff_table |>
    dplyr::arrange(var1, var2)

  expect_equal(actual_diff, expected_diff)
  # Verify no diagonal elements
  expect_true(!any(actual_diff$var1 == actual_diff$var2))
  expect_equal(
    corr$correlation_fit,
    sqrt(sum(expected_diff$difference ^ 2)) / n_nonzero_cells
  )
  expect_equal(
    corr$correlation_difference_mae,
    mean(abs(expected_diff$difference))
  )
  expect_equal(
    corr$correlation_difference_rmse,
    sqrt(mean(expected_diff$difference ^ 2))
  )
})

test_that("util_corr_fit works with NA ", {
  
  ed <- eval_data(synth_data = acs_conf, conf_data = acs_conf)
  
  corr <- util_corr_fit(eval_data = ed, use = "pairwise.complete.obs", method = "pearson")

  actual_diff <- corr$correlation_difference |>
    dplyr::select(var1, var2, difference) |>
    dplyr::arrange(var1, var2)
  
  # Verify no diagonal elements
  expect_true(!any(actual_diff$var1 == actual_diff$var2))
  expect_equal(max(corr$correlation_difference$difference, na.rm = TRUE), 0)
  expect_equal(corr$correlation_fit, 0)
  expect_equal(corr$correlation_difference_mae, 0)
  expect_equal(corr$correlation_difference_rmse, 0)
})

test_that("util_corr_fit works with group_by_q", {
  
  ed <- eval_data(synth_data = acs_conf, conf_data = acs_conf)
  
  corr <- util_corr_fit(eval_data = ed, use = "pairwise.complete.obs", group_by_q = "marst", method = "pearson")

  actual_diff <- corr$correlation_difference |>
    dplyr::select(marst, var1, var2, difference) |>
    dplyr::arrange(marst, var1, var2)

  # Verify no diagonal elements
  expect_true(!any(actual_diff$var1 == actual_diff$var2))
  expect_equal(max(corr$correlation_difference$difference, na.rm = TRUE), 0)
  expect_equal(max(corr$correlation_difference_mae$correlation_difference_mae, na.rm = TRUE), 0)
  expect_equal(max(corr$correlation_difference_rmse$correlation_difference_rmse, na.rm = TRUE), 0)
  expect_equal(max(corr$correlation_fit$correlation_fit, na.rm = TRUE), 0)
})

test_that("util_corr_fit works with spearman method", {

  ed <- eval_data(synth_data = acs_conf, conf_data = acs_conf)

  corr <- util_corr_fit(
    eval_data = ed,
    use = "pairwise.complete.obs",
    method = "spearman"
  )

  actual_diff <- corr$correlation_difference |>
    dplyr::select(var1, var2, difference) |>
    dplyr::arrange(var1, var2)

  expect_true(!any(actual_diff$var1 == actual_diff$var2))
  expect_equal(max(corr$correlation_difference$difference, na.rm = TRUE), 0)
  expect_equal(corr$correlation_fit, 0)
  expect_equal(corr$correlation_difference_mae, 0)
  expect_equal(corr$correlation_difference_rmse, 0)

})

test_that("util_corr_fit handles different conf/syn with NA values", {

  conf2 <- data.frame(
    x = c(1, 2, NA, 4, 5),
    y = c(2, NA, 6, 8, 10),
    z = c(5, 4, 3, NA, 1),
    w = c(NA, NA, 1, NA, NA),   # forces NA correlations with pairwise.complete.obs
    grp = c("A", "A", "B", "B", "B")
  )

  syn2 <- data.frame(
    x = c(1, 3, NA, 4, 6),
    y = c(1, NA, 7, 7, 11),
    z = c(5, 5, 2, NA, 0),
    w = c(NA, NA, 2, NA, NA),
    grp = c("A", "A", "B", "B", "B")
  )

  ed2 <- eval_data(conf_data = conf2, synth_data = syn2)

  corr2 <- util_corr_fit(ed2, use = "pairwise.complete.obs")

  expect_true(!any(corr2$correlation_difference$var1 == corr2$correlation_difference$var2))
  expect_true(any(is.na(corr2$correlation_difference$difference)))
  expect_false(is.na(corr2$correlation_fit))
  expect_false(is.na(corr2$correlation_difference_mae))
  expect_false(is.na(corr2$correlation_difference_rmse))
  expect_gt(corr2$correlation_fit, 0)
})

test_that("util_corr_fit correctly handles eval_data objects with <2 common numeric variables", {

  conf3 <- data.frame(
    x = c(1, 2, NA, 4, 5),
    z = c(5, 4, 3, NA, 1),
    grp = c("A", "A", "B", "B", "B")
  )

  syn3 <- data.frame(
    x = c(1, 3, NA, 4, 6),
    y = c(1, NA, 7, 7, 11),
    grp = c("A", "A", "B", "B", "B")
  )

  ed3 <- eval_data(conf_data = conf3, synth_data = syn3)

  # ungrouped
  corr3a <- util_corr_fit(ed3, use = "pairwise.complete.obs")
  expect_named(corr3a$correlation_original, c("var1", "var2", "correlation"))
  expect_named(corr3a$correlation_synthetic, c("var1", "var2", "correlation"))
  expect_named(corr3a$correlation_difference, c("var1", "var2", "difference"))
  expect_equal(nrow(corr3a$correlation_difference), 0)
  expect_equal(corr3a$correlation_fit, NA_real_)
  expect_equal(corr3a$correlation_difference_mae, NA_real_)
  expect_equal(corr3a$correlation_difference_rmse, NA_real_)

  # grouped
  corr3b <- util_corr_fit(ed3, use = "pairwise.complete.obs", group_by_q = "grp")
  expect_true("grp" %in% names(corr3b$correlation_original))
  expect_true("grp" %in% names(corr3b$correlation_synthetic))
  expect_true("grp" %in% names(corr3b$correlation_difference))
  expect_equal(nrow(corr3b$correlation_difference), 0)
  expect_equal(corr3b$correlation_fit, NA_real_)
  expect_equal(corr3b$correlation_difference_mae, NA_real_)
  expect_equal(corr3b$correlation_difference_rmse, NA_real_)
  
})
