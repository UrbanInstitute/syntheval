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
  
})

test_that("plot_cormat creates the right ggplot", {
  
  plot <- plot_cormat(conf_df, synth_df)
  
  expect_equal(length(plot$grobs), 2)
  
})