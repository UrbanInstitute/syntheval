test_that("discrimination() returns the correct object " , {
  
  discrimination <- discrimination(penguins_postsynth, penguins_conf)
  
  expect_equal(nrow(discrimination$combined_data), nrow(penguins_postsynth$synthetic_data) + nrow(penguins_conf))
  
  expect_equal(ncol(discrimination$combined_data), ncol(penguins_postsynth$synthetic_data) + 1)
  expect_equal(ncol(discrimination$combined_data), ncol(penguins_conf) + 1)
  
})

test_that("discrimination() returns the correct object when sysnthesizing a subset " , {
  
  postsynth_narrow <- penguins_postsynth
  postsynth_narrow$synthetic_data <- dplyr::select(postsynth_narrow$synthetic_data, -bill_depth_mm)
  
  # expect warning for mismatched columns
  expect_message(discrimination(postsynth_narrow, penguins_conf))
  
  # create discrimination to test dimensions
  discrimination <- suppressMessages(discrimination(postsynth_narrow, penguins_conf))
  
  expect_equal(nrow(discrimination$combined_data), nrow(postsynth_narrow$synthetic_data) + nrow(penguins_conf))
  
  expect_equal(ncol(discrimination$combined_data), ncol(postsynth_narrow$synthetic_data) + 1)
  # still contains the dropped column
  expect_equal(ncol(discrimination$combined_data), ncol(penguins_conf))
  
})



