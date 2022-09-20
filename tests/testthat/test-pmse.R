test_that("pmse is 0 for copied data ", {
  
  pmse <- pmse(
    data_orig = mtcars, 
    data_syn = mtcars
  )
  
  expect_equal(pmse$pmse, 0)
  expect_null(pmse$variable_importance)
  
})


test_that("pmse returns correct outputs ", {
  
  pmse <- pmse(
    data_orig = mtcars, 
    data_syn = dplyr::slice_sample(mtcars[1, ], n = 32, replace = TRUE)
  )
  
  expect_vector(pmse$pmse)
  expect_vector(pmse$variable_importance)
  
})
