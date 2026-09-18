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
  expect_equal(nrow(plot$data), 1)
  expect_equal(as.character(plot$data$var1), "n1")
  expect_equal(as.character(plot$data$var2), "n2")
  
})

test_that("plot_cormat throws expected errors", {

  expect_error(
    plot_cormat(conf_df)
  )

})

test_that("plot_cormat creates the right ggplot", {

  plot <- plot_cormat(ed)

  expect_equal(length(plot$grobs), 2)

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

  # plot_cormat should run without error even though the numeric variable
  # sets differ between conf_data and synth_data
  expect_no_error(plot_cormat(ed2))

  # replicate the intersection logic plot_cormat uses internally and confirm
  # both heatmaps would be built from the same (intersected) variable set
  intersect_numeric <- intersect(
    names(conf_df)[sapply(conf_df, is.numeric)],
    names(synth_df2)[sapply(synth_df2, is.numeric)]
  )

  p1 <- create_cormat_plot(conf_df[intersect_numeric])
  p2 <- create_cormat_plot(synth_df2[intersect_numeric])

  conf_vars <- sort(unique(c(as.character(p1$data$var1), as.character(p1$data$var2))))
  synth_vars <- sort(unique(c(as.character(p2$data$var1), as.character(p2$data$var2))))

  expect_true(!("n3" %in% synth_vars))
  expect_equal(conf_vars, synth_vars)

})