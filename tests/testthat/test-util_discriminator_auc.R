


data <- tibble::tibble(a = c(1, 1, 2, 2))


postsynth <- list(
  synthetic_data = tibble::tibble(a = c(1, 1, 2, 2))
)

test_that("Identical data lead to AUC = 0.5", {

  expect_equal(
    util_discriminator_auc(postsynth = postsynth, data = data)$auc, 
    0.5
  )
  
  expect_equal(
    util_discriminator_auc(postsynth = postsynth, data = data, model = "random forest")$auc, 
    0.5
  )
  
})
