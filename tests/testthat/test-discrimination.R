
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


test_that("print.discrimination reports what has been computed", {

  ed <- eval_data(conf_data = penguins_conf, synth_data = penguins_postsynth)
  disc <- discrimination(ed)

  expect_output(print(disc), regexp = "Discrimination object")
  expect_output(print(disc), regexp = "Discriminator: not fitted")
  expect_output(print(disc), regexp = "pMSE: not computed")
  expect_output(print(disc), regexp = "pMSE ratio: not computed")
  expect_output(print(disc), regexp = "SPECKS: not computed")
  expect_output(print(disc), regexp = "Discriminator AUC: not computed")
  expect_invisible(print(disc))

  set.seed(1)

  logistic_mod <- parsnip::logistic_reg() |>
    parsnip::set_mode(mode = "classification") |>
    parsnip::set_engine(engine = "glm")

  rec <- recipes::recipe(.source_label ~ ., data = disc$combined_data)

  disc <- disc |>
    add_propensities(recipe = rec, spec = logistic_mod) |>
    add_pmse(split = FALSE)

  expect_output(print(disc), regexp = "Discriminator: workflow")
  expect_output(print(disc), regexp = "pMSE: computed")
  expect_output(print(disc), regexp = "pMSE ratio: not computed")

  disc <- add_pmse_ratio(disc, split = FALSE, times = 5)

  expect_output(print(disc), regexp = "pMSE ratio: computed")

})

test_that("summary.discrimination returns a stable three-column tibble", {

  ed <- eval_data(conf_data = penguins_conf, synth_data = penguins_postsynth)
  disc <- discrimination(ed)

  empty <- summary(disc)

  expect_s3_class(empty, "tbl_df")
  expect_equal(nrow(empty), 0)
  expect_named(empty, c(".metric", ".sample", ".value"))

  set.seed(1)

  logistic_mod <- parsnip::logistic_reg() |>
    parsnip::set_mode(mode = "classification") |>
    parsnip::set_engine(engine = "glm")

  rec <- recipes::recipe(.source_label ~ ., data = disc$combined_data)

  disc <- disc |>
    add_propensities(recipe = rec, spec = logistic_mod) |>
    add_pmse() |>
    add_specks() |>
    add_discriminator_auc()

  out <- summary(disc)

  expect_named(out, c(".metric", ".sample", ".value"))
  expect_setequal(unique(out$.metric), c("pmse", "specks", "discriminator_auc"))
  expect_setequal(unique(out$.sample), c("training", "testing"))
  expect_equal(nrow(out), 6)

  expect_equal(
    out$.value[out$.metric == "pmse" & out$.sample == "training"],
    disc$pmse$.pmse[disc$pmse$.source == "training"]
  )

  expect_equal(
    out$.value[out$.metric == "specks" & out$.sample == "testing"],
    disc$specks$.specks[disc$specks$.source == "testing"]
  )

  expect_equal(
    out$.value[out$.metric == "discriminator_auc" & out$.sample == "testing"],
    disc$discriminator_auc$.estimate[disc$discriminator_auc$.sample == "testing"]
  )

  disc <- add_pmse_ratio(disc, times = 5)
  out <- summary(disc)

  expect_setequal(
    unique(out$.metric),
    c("pmse", "null_pmse", "pmse_ratio", "specks", "discriminator_auc")
  )
  expect_equal(nrow(out), 10)

  expect_equal(
    out$.value[out$.metric == "pmse_ratio" & out$.sample == "testing"],
    disc$pmse$.pmse_ratio[disc$pmse$.source == "testing"]
  )

})

test_that("summary.discrimination uses 'overall' when split = FALSE", {

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
    add_specks(split = FALSE) |>
    add_discriminator_auc(split = FALSE)

  out <- summary(disc)

  expect_equal(unique(out$.sample), "overall")
  expect_equal(nrow(out), 3)

})


