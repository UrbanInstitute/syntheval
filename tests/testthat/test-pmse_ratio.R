library(dplyr)

postsynth <- penguins_syn

test_that("Is the pMSE ratio 0 for identical data?", {
  
  set.seed(1)
  pmse_ratio <- pmse_ratio(
    postsynth = postsynth, 
    data = postsynth$synthetic_data, 
    formula = id ~ .,
    times = 1, 
    cp = 0.01, 
    null_pmse = NULL
  )
  
  expect_equal(pmse_ratio$pmse_ratio, 0)
  expect_equal(pmse_ratio$pmse, 0)
  
})

test_that("Does null_pmse work with a null-pMSE?", {
  
  set.seed(1)
  pmse_ratio <- pmse_ratio(
    postsynth = postsynth, 
    data = postsynth$synthetic_data, 
    null_pmse = 0.02103253
  )
  
  expect_equal(round(pmse_ratio$pmse_ratio, digits = 5), 0)
  
})

test_that("Does null-pMSE work for a subset of variables", {
  
  set.seed(1)
  pmse_ratio1 <- pmse_ratio(
    postsynth = postsynth, 
    data = penguins_conf, 
    formula = id ~ bill_length_mm + bill_depth_mm,
    times = 10
  )
  
  set.seed(1)
  pmse_ratio2 <- pmse_ratio(
    postsynth = postsynth, 
    data = penguins_conf, 
    formula = id ~ bill_length_mm + bill_depth_mm,
    times = 10
  )
  
  expect_equal(pmse_ratio1, pmse_ratio2)
  
})
