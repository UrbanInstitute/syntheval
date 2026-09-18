test_that("add_pmse errors on non-discrimination input", {

    expect_error(add_pmse(list()), regexp = "discrimination object")

})

test_that("add_pmse errors when propensities are missing", {

    ed <- eval_data(conf_data = penguins_conf, synth_data = penguins_postsynth)

    expect_error(add_pmse(discrimination(ed)), regexp = "add_propensities\\(\\)")

})