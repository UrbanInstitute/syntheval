
test_that("discrimination() returns the correct object " , {

  ed <- eval_data(conf_data = penguins_conf, synth_data = penguins_postsynth)

  discrimination <- discrimination(ed)

  expect_equal(nrow(discrimination$combined_data), nrow(penguins_postsynth$synthetic_data) + nrow(penguins_conf))

  expect_equal(ncol(discrimination$combined_data), ncol(penguins_postsynth$synthetic_data) + 1)
  expect_equal(ncol(discrimination$combined_data), ncol(penguins_conf) + 1)

})

test_that("discrimination() returns the correct object when sysnthesizing a subset " , {

  postsynth_narrow <- penguins_postsynth
  postsynth_narrow$synthetic_data <- dplyr::select(postsynth_narrow$synthetic_data, -bill_depth_mm)

  ed1 <- eval_data(conf_data = penguins_conf, synth_data = postsynth_narrow)

  # expect warning for mismatched columns
  expect_message(
    discrimination <- discrimination(ed1)
  )

  expect_equal(nrow(discrimination$combined_data), nrow(postsynth_narrow$synthetic_data) + nrow(penguins_conf))

  expect_equal(ncol(discrimination$combined_data), ncol(postsynth_narrow$synthetic_data) + 1)
  # still contains the dropped column
  expect_equal(ncol(discrimination$combined_data), ncol(penguins_conf))

})

test_that(".validate_discrimination passes a valid object through invisibly", {

  ed <- eval_data(conf_data = penguins_conf, synth_data = penguins_postsynth)
  disc <- discrimination(ed)

  expect_no_error(.validate_discrimination(disc))
  expect_invisible(.validate_discrimination(disc))
  expect_identical(.validate_discrimination(disc), disc)
})


test_that("print.discrimination shows the computed metric values", {

  ed <- eval_data(conf_data = penguins_conf, synth_data = penguins_postsynth)
  disc <- discrimination(ed)

  # nothing fitted or computed yet
  expect_output(print(disc), regexp = "^Discrimination\\n")
  expect_output(print(disc), regexp = "original: 333, synthetic: 333")
  expect_output(print(disc), regexp = "Discriminator: not fitted")
  expect_output(print(disc), regexp = "Metrics: none computed")
  expect_invisible(print(disc))

  set.seed(1)

  logistic_mod <- parsnip::logistic_reg() |>
    parsnip::set_mode(mode = "classification") |>
    parsnip::set_engine(engine = "glm")

  rec <- recipes::recipe(.source_label ~ ., data = disc$combined_data)

  disc <- disc |>
    add_propensities(recipe = rec, spec = logistic_mod) |>
    add_discriminator_auc() |>
    add_specks() |>
    add_pmse()

  out <- utils::capture.output(print(disc))

  expect_true(any(grepl("Discriminator: logistic_reg \\(glm\\), fitted", out)))
  expect_false(any(grepl("none computed", out)))

  # column headers for the split
  expect_true(any(grepl("training", out) & grepl("testing", out)))

  # rows appear in README order, and pmse ratio rows are absent before
  # add_pmse_ratio() runs
  auc_row <- grep("^Discriminator AUC", out)
  specks_row <- grep("^SPECKS", out)
  pmse_row <- grep("^pMSE\\s+[0-9]", out)

  expect_length(auc_row, 1)
  expect_length(specks_row, 1)
  expect_length(pmse_row, 1)
  expect_true(auc_row < specks_row && specks_row < pmse_row)
  expect_false(any(grepl("pMSE ratio", out)))

  # the printed AUC matches the element, to 3 significant digits
  auc_training <- disc$discriminator_auc$.estimate[disc$discriminator_auc$.sample == "training"]
  expect_true(grepl(formatC(auc_training, digits = 3, format = "g"), out[auc_row], fixed = TRUE))

  disc <- add_pmse_ratio(disc, times = 5)
  out <- utils::capture.output(print(disc))

  expect_true(any(grepl("^null pMSE", out)))
  expect_true(any(grepl("^pMSE ratio", out)))
  expect_true(grep("^pMSE\\s+[0-9]", out) < grep("^null pMSE", out))
  expect_true(grep("^null pMSE", out) < grep("^pMSE ratio", out))

})

test_that("print.discrimination uses an 'overall' column when split = FALSE", {

  ed <- eval_data(conf_data = penguins_conf, synth_data = penguins_postsynth)
  disc <- discrimination(ed)

  set.seed(1)

  logistic_mod <- parsnip::logistic_reg() |>
    parsnip::set_mode(mode = "classification") |>
    parsnip::set_engine(engine = "glm")

  rec <- recipes::recipe(.source_label ~ ., data = disc$combined_data)

  disc <- disc |>
    add_propensities(recipe = rec, spec = logistic_mod) |>
    add_pmse(split = FALSE) |>
    add_specks(split = FALSE)

  out <- utils::capture.output(print(disc))

  expect_true(any(grepl("overall", out)))
  expect_false(any(grepl("training", out)))

})

test_that(".discrimination_metrics returns an empty four-column tibble before any metric is added", {

  ed <- eval_data(conf_data = penguins_conf, synth_data = penguins_postsynth)
  disc <- discrimination(ed)

  empty <- .discrimination_metrics(disc)

  expect_s3_class(empty, "tbl_df")
  expect_equal(nrow(empty), 0)
  expect_named(empty, c(".metric", ".label", ".sample", ".value"))

})

test_that(".discrimination_metrics orders rows by metric label, not by the order add_*() was called", {

  ed <- eval_data(conf_data = penguins_conf, synth_data = penguins_postsynth)
  disc <- discrimination(ed)

  set.seed(1)

  logistic_mod <- parsnip::logistic_reg() |>
    parsnip::set_mode(mode = "classification") |>
    parsnip::set_engine(engine = "glm")

  rec <- recipes::recipe(.source_label ~ ., data = disc$combined_data)

  # add in the opposite order to the display order
  disc <- disc |>
    add_propensities(recipe = rec, spec = logistic_mod) |>
    add_pmse() |>
    add_pmse_ratio(times = 5) |>
    add_specks() |>
    add_discriminator_auc()

  out <- .discrimination_metrics(disc)

  expect_named(out, c(".metric", ".label", ".sample", ".value"))
  expect_equal(unique(out$.label), c("Discriminator AUC", "SPECKS", "pMSE", "null pMSE", "pMSE ratio"))
  expect_equal(nrow(out), 10)

  # display order is AUC, SPECKS, pMSE, null pMSE, pMSE ratio, with training
  # before testing within each metric
  expect_equal(
    out$.metric,
    rep(c("discriminator_auc", "specks", "pmse", "null_pmse", "pmse_ratio"), each = 2)
  )
  expect_equal(out$.sample, rep(c("training", "testing"), times = 5))

  # values are copied from the element tibbles unchanged
  expect_equal(
    out$.value[out$.metric == "pmse_ratio" & out$.sample == "testing"],
    disc$pmse$.pmse_ratio[disc$pmse$.source == "testing"]
  )

})
