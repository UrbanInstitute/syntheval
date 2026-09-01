# ---- fixtures ---------------------------------------------------------------
#
#   conf               synth
#     a  b               a  b
#     x  p               x  p
#     x  p               y  q
#     y  p               y  q
#     y  q               y  p
#
#   marginal a: conf (x = 0.50, y = 0.50), synth (x = 0.25, y = 0.75)
#   marginal b: conf (p = 0.75, q = 0.25), synth (p = 0.50, q = 0.50)

conf <- tibble::tibble(
  a = c("x", "x", "y", "y"),
  b = c("p", "p", "p", "q")
)

synth <- tibble::tibble(
  a = c("x", "y", "y", "y"),
  b = c("p", "q", "q", "p")
)

combos_1 <- matrix(c("a", "b"), ncol = 1)

# ---- cell proportions -------------------------------------------------------

test_that("cells report proportions and differences per combination", {
  cells <- .compute_marginal_cells(
    synth_data = synth, conf_data = conf, combos = combos_1
  )

  expect_named(
    cells,
    c("variables", "cell", "prop_synth", "prop_conf", "abs_diff", "diff")
  )

  # one row per observed level of each single-variable marginal
  expect_equal(nrow(cells), 4)
  expect_equal(unique(cells$variables), c("a", "b"))

  x_cell <- dplyr::filter(cells, .data$variables == "a", .data$cell == "x")

  expect_equal(x_cell$prop_synth, 0.25)
  expect_equal(x_cell$prop_conf, 0.5)
  expect_equal(x_cell$diff, -0.25)
  expect_equal(x_cell$abs_diff, 0.25)
})

test_that("cells absent from one dataset count as proportion zero", {
  synth_x_only <- tibble::tibble(a = c("x", "x"))
  conf_xy <- tibble::tibble(a = c("x", "y"))

  cells <- .compute_marginal_cells(
    synth_data = synth_x_only, conf_data = conf_xy, combos = matrix("a")
  )

  y_cell <- dplyr::filter(cells, .data$cell == "y")

  expect_equal(y_cell$prop_synth, 0)
  expect_equal(y_cell$prop_conf, 0.5)
})

test_that("weighted proportions are weight shares", {
  # conf weights 3, 1 -> x = 0.75; synth unit weights -> x = 0.5
  conf_w <- tibble::tibble(a = c("x", "y"), w = c(3, 1))
  synth_w <- tibble::tibble(a = c("x", "y"), w = c(1, 1))

  cells <- .compute_marginal_cells(
    synth_data = synth_w,
    conf_data = conf_w,
    combos = matrix("a"),
    weight_var = "w"
  )

  x_cell <- dplyr::filter(cells, .data$cell == "x")

  expect_equal(x_cell$prop_conf, 0.75)
  expect_equal(x_cell$prop_synth, 0.5)
})

# ---- missing values ---------------------------------------------------------

test_that("na.rm drops rows per marginal, not globally", {
  # a has an NA; b is complete, so its proportions must be unaffected
  conf_na <- dplyr::mutate(conf, a = c("x", "x", "y", NA))

  cells <- .compute_marginal_cells(
    synth_data = synth,
    conf_data = conf_na,
    combos = combos_1,
    na.rm = TRUE
  )

  # marginal a uses 3 conf rows: x = 2/3
  a_x <- dplyr::filter(cells, .data$variables == "a", .data$cell == "x")
  expect_equal(a_x$prop_conf, 2 / 3)

  # marginal b uses all 4 conf rows: p = 3/4
  b_p <- dplyr::filter(cells, .data$variables == "b", .data$cell == "p")
  expect_equal(b_p$prop_conf, 0.75)
})

test_that("weighted proportions drop missing rows before computing shares", {
  # na.rm = TRUE drops each dataset's NA row; shares use surviving weights
  #   conf keeps weights 1, 1, 2 -> x = 2/4, y = 2/4
  #   synth keeps weights 1, 3 -> x = 1/4, y = 3/4
  # dividing by the full total (including the dropped 10s) would differ
  conf_wna <- tibble::tibble(a = c("x", "x", "y", NA), w = c(1, 1, 2, 10))
  synth_wna <- tibble::tibble(a = c("x", "y", NA), w = c(1, 3, 10))

  cells <- .compute_marginal_cells(
    synth_data = synth_wna,
    conf_data = conf_wna,
    combos = matrix("a"),
    weight_var = "w",
    na.rm = TRUE
  )

  x_cell <- dplyr::filter(cells, .data$cell == "x")

  expect_equal(x_cell$prop_conf, 0.5)
  expect_equal(x_cell$prop_synth, 0.25)
})

test_that("only the synthetic side may be empty after na.rm", {
  synth_empty <- tibble::tibble(a = c(NA_character_, NA_character_))
  conf_xy <- tibble::tibble(a = c("x", "y"))

  # an empty synthetic marginal scores against zero proportions when allowed
  cells <- .compute_marginal_cells(
    synth_data = synth_empty,
    conf_data = conf_xy,
    combos = matrix("a"),
    na.rm = TRUE,
    allow_empty_synth = TRUE
  )

  expect_equal(cells$prop_synth, c(0, 0))
  expect_equal(cells$prop_conf, c(0.5, 0.5))

  # and errors when not allowed
  expect_error(
    .compute_marginal_cells(
      synth_data = synth_empty,
      conf_data = conf_xy,
      combos = matrix("a"),
      na.rm = TRUE,
      allow_empty_synth = FALSE
    ),
    regexp = "no rows remain"
  )

  # the confidential side has nothing to score against and always errors
  expect_error(
    .compute_marginal_cells(
      synth_data = conf_xy,
      conf_data = synth_empty,
      combos = matrix("a"),
      na.rm = TRUE,
      allow_empty_synth = TRUE
    ),
    regexp = "no rows remain"
  )
})
