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
test_that("weight_var validation pins error messages", {
  synth <- tibble::tibble(a = c("x", "y"), w = c(1, 1))
  conf <- tibble::tibble(a = c("x", "y"), w = c(1, 1))

  prepare <- function(synth_data = synth, conf_data = conf, ...) {
    .prepare_k_marginals_inputs(
      synth_data = synth_data, conf_data = conf_data, k = 1, ...
    )
  }

  expect_error(
    prepare(weight_var = 1),
    regexp = "`weight_var` must be a single character string"
  )

  expect_error(
    prepare(synth_data = dplyr::select(synth, -w), weight_var = "w"),
    regexp = "`weight_var` must be a column in both datasets"
  )

  expect_error(
    prepare(conf_data = dplyr::mutate(conf, w = "1"), weight_var = "w"),
    regexp = "`weight_var` must be a numeric column in both datasets"
  )

  # bad values are rejected on either side
  bad_weights <- list(
    finite = c(1, Inf),
    finite = c(1, NA),
    "non-negative" = c(1, -1),
    "positive total" = c(0, 0)
  )

  for (i in seq_along(bad_weights)) {
    expect_error(
      prepare(conf_data = dplyr::mutate(conf, w = bad_weights[[i]]), weight_var = "w"),
      regexp = names(bad_weights)[i]
    )

    expect_error(
      prepare(synth_data = dplyr::mutate(synth, w = bad_weights[[i]]), weight_var = "w"),
      regexp = names(bad_weights)[i]
    )
  }
})

test_that("group_by validation pins error messages", {
  synth <- tibble::tibble(g = c("A", "B"), a = c("x", "y"), w = c(1, 1))
  conf <- synth

  prepare <- function(...) {
    .prepare_k_marginals_inputs(synth_data = synth, conf_data = conf, k = 1, ...)
  }

  for (bad in list(1, "zzz", character(0), c("g", NA))) {
    expect_error(
      prepare(group_by = bad),
      regexp = "`group_by` must be a character vector"
    )
  }

  expect_error(
    prepare(group_by = c("g", "g")),
    regexp = "must not contain duplicate"
  )

  expect_error(
    prepare(group_by = "w", weight_var = "w"),
    regexp = "`group_by` cannot include `weight_var`"
  )

  # k is checked against the universe without the grouping variables
  expect_error(
    .prepare_k_marginals_inputs(
      synth_data = synth, conf_data = conf, k = 2, group_by = "g", weight_var = "w"
    ),
    regexp = "`k` cannot exceed"
  )
})

test_that("synth_varnames validation pins error messages", {
  synth <- tibble::tibble(a = c("x", "y"), b = c("p", "q"))
  conf <- synth

  for (bad in list(character(0), NA_character_, c("a", NA), 1)) {
    expect_error(
      .prepare_k_marginals_inputs(
        synth_data = synth, conf_data = conf, k = 1, synth_varnames = bad
      ),
      regexp = "`synth_varnames` must be a non-empty character vector"
    )
  }

  expect_error(
    .prepare_k_marginals_inputs(
      synth_data = synth, conf_data = conf, k = 1, synth_varnames = "zzz"
    ),
    regexp = "`synth_varnames` matches no variables available"
  )

  expect_error(
    .prepare_k_marginals_inputs(
      synth_data = synth, conf_data = conf, k = 2, synth_varnames = "a"
    ),
    regexp = "`k` cannot exceed"
  )
})

test_that("missing-data handling messages and errors", {
  synth <- tibble::tibble(a = c("x", "y"), b = c("p", "q"), c = c("m", "n"))
  conf <- dplyr::mutate(synth, a = c("x", NA), b = c(NA, "q"))

  # the message lists every affected variable
  expect_message(
    .prepare_k_marginals_inputs(synth_data = synth, conf_data = conf, k = 1),
    regexp = "contain missing data: a, b"
  )

  # variables outside the marginal universe do not drive NA handling
  expect_no_message(
    result <- .prepare_k_marginals_inputs(
      synth_data = synth, conf_data = conf, k = 1, synth_varnames = "c"
    )
  )

  # a literal "NA" level collides with the NA-as-level conversion
  conf_lit <- tibble::tibble(a = c("NA", "x", NA), g = c("A", "A", "A"))
  synth_lit <- tibble::tibble(a = c("x", "x", "x"), g = c("NA", "A", NA))

  expect_error(
    suppressMessages(
      .prepare_k_marginals_inputs(synth_data = synth, conf_data = conf_lit, k = 1)
    ),
    regexp = "'NA' already exists"
  )

  expect_error(
    suppressMessages(
      .prepare_k_marginals_inputs(
        synth_data = synth_lit, conf_data = conf_lit, k = 1, group_by = "g"
      )
    ),
    regexp = "'NA' already exists"
  )

  # na.rm = TRUE must leave some confidential rows after dropping NA groups
  conf_gna <- tibble::tibble(g = c(NA, NA), a = c("x", "y"))

  expect_error(
    .prepare_k_marginals_inputs(
      synth_data = synth_lit, conf_data = conf_gna, k = 1, group_by = "g",
      na.rm = TRUE
    ),
    regexp = "no confidential rows remain"
  )
})
