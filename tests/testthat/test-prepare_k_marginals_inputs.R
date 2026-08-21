test_that("shared vars exclude grouping and weight columns", {
  synth <- tibble::tibble(a = "x", b = "p", g = "A", w = 1)
  conf <- tibble::tibble(a = "x", b = "p", g = "A", w = 2)

  result <- .prepare_k_marginals_inputs(
    synth_data = synth,
    conf_data = conf,
    k = 1,
    weight_var = "w",
    group_by = "g"
  )

  expect_equal(result$shared_vars, c("a", "b"))
})

test_that("synth_varnames restrict the shared variable universe", {
  synth <- tibble::tibble(a = "x", b = "p", c = "m")
  conf <- tibble::tibble(a = "x", b = "p", c = "m")

  result <- .prepare_k_marginals_inputs(
    synth_data = synth,
    conf_data = conf,
    k = 1,
    synth_varnames = c("a", "c")
  )

  expect_equal(result$shared_vars, c("a", "c"))
})

test_that("na.rm drops rows missing a grouping value", {
  synth <- tibble::tibble(g = c("A", NA), a = c("x", "y"))
  conf <- tibble::tibble(g = c("A", NA), a = c("x", "y"))

  result <- .prepare_k_marginals_inputs(
    synth_data = synth,
    conf_data = conf,
    k = 1,
    group_by = "g",
    na.rm = TRUE
  )

  expect_equal(nrow(result$synth_data), 1)
  expect_equal(nrow(result$conf_data), 1)
  expect_equal(result$synth_data$g, "A")
  expect_equal(result$conf_data$g, "A")
})