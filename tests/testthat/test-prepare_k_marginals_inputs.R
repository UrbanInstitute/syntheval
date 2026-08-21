# ---- fixtures ---------------------------------------------------------------
#
# two marginal variables a and b, a grouping variable g, and a weight w

conf <- tibble::tibble(
  a = c("x", "x", "y", "y"),
  b = c("p", "p", "p", "q"),
  g = c("A", "A", "B", "B"),
  w = c(1, 1, 1, 1)
)

synth <- dplyr::mutate(conf, a = c("x", "y", "y", "y"))

prepare <- function(
  synth_data = synth,
  conf_data = conf,
  k = 1,
  ...
) {
  .prepare_k_marginals_inputs(
    synth_data = synth_data,
    conf_data = conf_data,
    k = k,
    ...
  )
}

# ---- shared variables -------------------------------------------------------

test_that("shared vars exclude grouping and weight columns", {
  result <- prepare(weight_var = "w", group_by = "g")

  expect_equal(result$shared_vars, c("a", "b"))
})

test_that("synth_varnames restrict the shared variable universe", {
  result <- prepare(synth_varnames = "a")

  expect_equal(result$shared_vars, "a")
})

# ---- missing values ---------------------------------------------------------

test_that("na.rm drops rows missing a grouping value", {
  conf_na_group <- dplyr::mutate(conf, g = c("A", "A", NA, NA))

  result <- prepare(
    synth_data = conf_na_group,
    conf_data = conf_na_group,
    group_by = "g",
    na.rm = TRUE
  )

  expect_equal(nrow(result$synth_data), 2)
  expect_equal(nrow(result$conf_data), 2)
  expect_equal(unique(result$conf_data$g), "A")
})

test_that("missing-data handling messages and errors", {
  conf_na <- dplyr::mutate(
    conf,
    a = c("x", NA, "y", "y"),
    b = c(NA, "p", "p", "q")
  )

  # the message lists every affected variable
  expect_message(
    prepare(conf_data = conf_na),
    regexp = "contain missing data: a, b"
  )

  # variables outside the marginal universe do not drive NA handling
  expect_no_message(prepare(conf_data = conf_na, synth_varnames = "g"))

  # a literal "NA" level collides with the NA-as-level conversion, whether
  # in a marginal variable or a grouping variable
  conf_literal <- dplyr::mutate(conf, a = c("NA", "x", NA, "y"))
  conf_literal_group <- dplyr::mutate(conf, g = c("NA", "A", NA, "B"))

  expect_error(
    suppressMessages(prepare(conf_data = conf_literal)),
    regexp = "'NA' already exists"
  )

  expect_error(
    suppressMessages(prepare(conf_data = conf_literal_group, group_by = "g")),
    regexp = "'NA' already exists"
  )

  # na.rm = TRUE must leave some confidential rows after dropping NA groups
  conf_all_na_group <- dplyr::mutate(conf, g = NA_character_)

  expect_error(
    prepare(conf_data = conf_all_na_group, group_by = "g", na.rm = TRUE),
    regexp = "no confidential rows remain"
  )
})

# ---- input errors -----------------------------------------------------------
#
# Exact messages are pinned here; test-util_k_marginals.R checks only that
# each rule errors.

test_that("weight_var validation pins error messages", {
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
    finite = c(1, 1, 1, Inf),
    finite = c(1, 1, 1, NA),
    "non-negative" = c(1, 1, 1, -1),
    "positive total" = c(0, 0, 0, 0)
  )

  for (i in seq_along(bad_weights)) {
    expect_error(
      prepare(
        conf_data = dplyr::mutate(conf, w = bad_weights[[i]]),
        weight_var = "w"
      ),
      regexp = names(bad_weights)[i]
    )

    expect_error(
      prepare(
        synth_data = dplyr::mutate(synth, w = bad_weights[[i]]),
        weight_var = "w"
      ),
      regexp = names(bad_weights)[i]
    )
  }
})

test_that("group_by validation pins error messages", {
  for (bad in list(
    1,
    "zzz",
    character(0),
    c("g", NA)
  )) {
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
    prepare(k = 3, group_by = "g", weight_var = "w"),
    regexp = "`k` cannot exceed"
  )
})

test_that("synth_varnames validation pins error messages", {
  for (bad in list(
    character(0),
    NA_character_,
    c("a", NA),
    1
  )) {
    expect_error(
      prepare(synth_varnames = bad),
      regexp = "`synth_varnames` must be a non-empty character vector"
    )
  }

  expect_error(
    prepare(synth_varnames = "zzz"),
    regexp = "`synth_varnames` matches no variables available"
  )

  expect_error(
    prepare(k = 2, synth_varnames = "a"),
    regexp = "`k` cannot exceed"
  )
})
