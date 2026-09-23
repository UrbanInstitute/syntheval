conf_df <- data.frame(
  n1 = c(1., 2., 3., 4.),
  n2 = c(5., 6., 7., 8.),
  c1 = factor(c("1", "1", "2", "1")),
  c2 = factor(c("a", "a", "a", "b"))
)

synth_df <- data.frame(
  n1 = c(1., 2., 4., 4.),
  n2 = c(5., 5., 7., 9.),
  c1 = factor(c("1", "2", "2", "1")),
  c2 = factor(c("a", "b", "a", "b"))
)

ed <- eval_data(conf_data = conf_df, synth_data = synth_df)

test_that("plot_numeric_hist_kde throws expected errors", {
  
  expect_error(
    plot_numeric_hist_kde(ed, "c1")
  )
  
  expect_error(
    plot_numeric_hist_kde(ed, "n1", "n2")
  )
  
  expect_error(
    plot_numeric_hist_kde(ed, "n1", "c1", "n2")
  )
  
})

test_that("plot_numeric_hist_kde creates the right ggplot", {
  
  plot <- plot_numeric_hist_kde(ed, "n1")
  expect_s3_class(plot$layers[[1]]$geom, "GeomBar")
  expect_s3_class(plot$layers[[2]]$geom, "GeomDensity")
  
})

test_that("plot_categorical_bar throws expected errors", {
  
  expect_error(
    plot_categorical_bar(ed, "n1")
  )
  
  expect_error(
    plot_categorical_bar(ed, "c1", "n2")
  )
  
  expect_error(
    plot_categorical_bar(ed, "c1", "c1", "n2")
  )
  
})

test_that("plot_categorical_bar creates the right ggplot", {
  
  plot <- plot_categorical_bar(ed, "c1")
  expect_s3_class(plot$layers[[1]]$geom, "GeomBar")
  
})

test_that("create_cormat_plot creates the right ggplot", {
  
  plot <- create_cormat_plot(synth_df)
  
  expect_s3_class(plot$layers[[1]]$geom, "GeomTile")
  expect_s3_class(plot$layers[[2]]$geom, "GeomText")
  
  # synth_df has only 2 numeric variables (n1, n2), so a true lower triangle
  # (no mirrored/duplicate pairings) should only have 1 row
  # we've also reversed the direction of the triangle and keep pairings where
  # var1 > var2 so we expect var1 == n2 and var2 == n1
  expect_equal(nrow(plot$data), 1)
  expect_equal(as.character(plot$data$var1), "n2")
  expect_equal(as.character(plot$data$var2), "n1")
  
})

test_that("plot_cormat throws expected errors", {

  expect_error(
    plot_cormat(conf_df)
  )

})

test_that("plot_cormat creates the right ggplot", {

  plot <- plot_cormat(ed)

  expect_equal(length(plot), 2)

})

test_that("plot_cormat uses the same variable set for conf and synth plots", {

  # synth_df2 has an extra numeric variable (n3) not present in conf_df
  synth_df2 <- data.frame(
    n1 = c(1., 2., 4., 4.),
    n2 = c(5., 5., 7., 9.),
    n3 = c(2., 4., 6., 8.),
    c1 = factor(c("1", "2", "2", "1")),
    c2 = factor(c("a", "b", "a", "b"))
  )

  ed2 <- eval_data(conf_data = conf_df, synth_data = synth_df2)

  captured <- list()

  testthat::local_mocked_bindings(
    create_cormat_plot = function(data, cor_method = "pearson", group_by_q = NULL) {
      captured[[length(captured) + 1]] <<- names(data)
      ggplot2::ggplot()
    },
    .package = "syntheval"
  )
  expect_no_error(plot_cormat(ed2))

  expected <- intersect(
    names(conf_df)[sapply(conf_df, is.numeric)],
    names(synth_df2)[sapply(synth_df2, is.numeric)]
  )

  expect_length(captured, 2)
  expect_equal(captured[[1]], expected)
  expect_equal(captured[[2]], expected)

})

test_that("plot_cormat works with grouping variable", {

  captured_group <- list()

  label_group <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.character(x)) return(x[[1]])
    tryCatch(rlang::as_label(x), error = function(e) as.character(x)[1])
  }

  testthat::local_mocked_bindings(
    create_cormat_plot = function(data, cor_method = "pearson", group_by_q = NULL) {
      captured_group[[length(captured_group) + 1]] <<- group_by_q
      ggplot2::ggplot()
    },
    .package = "syntheval"
  )

  expect_no_error(plot_cormat(ed, group_by_q = "c1"))
  expect_length(captured_group, 2)

  # grouping argument should be forwarded both times
  expect_false(any(vapply(captured_group, is.null, logical(1))))
  expect_match(label_group(captured_group[[1]]), "c1")
  expect_match(label_group(captured_group[[2]]), "c1")

})
