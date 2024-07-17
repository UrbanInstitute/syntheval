data1 <- tibble::tibble(
  a = c(1, 1, 1, 1),
  b = c(1, 1, 1, 1)
)

data2 <- tibble::tibble(
  a = c(2, 2, 2, 2),
  b = c(2, 2, 2, 2)
)

data3 <- tibble::tibble(
  a = c(1, 1, 2, 2),
  b = c(1, 1, 2, 2)
)

postsynth <- list(
  synthetic_data = tibble::tibble(
    a = c(1, 1, 1, 1),
    b = c(1, 1, 1, 1)
    )
)

class(postsynth) <- "postsynth"



test_that("Perfect training match and perfect holdout lack of match", {

  test1 <- disc_mit(
    postsynth = postsynth,
    data = data1, 
    holdout_data = data2
  )
  
  
  expect_equal(
    test1$precision, 
    1
  )
  
  expect_equal(
    test1$recall, 
    1
  )
  
  expect_equal(
    test1$auc, 
    1
  )
  
})

test_that("Perfect training lack of match and perfect holdout match", {
  
  test2 <- disc_mit(
    postsynth = postsynth,
    data = data2, 
    holdout_data = data1
  )
  
  
  expect_equal(
    test2$precision, 
    0
  )
  
  expect_equal(
    test2$recall, 
    0
  )
  
  expect_equal(
    test2$auc, 
    0
  )
  
})

test_that("Identical training and holdout data", {
  
  test3 <- disc_mit(
    postsynth = postsynth,
    data = data3, 
    holdout_data = data3
  )
  
  expect_equal(
    test3$precision, 
    0.5
  )
  
  expect_equal(
    test3$recall, 
    0.5
  )
  
  expect_equal(
    test3$auc, 
    0.5
  )
  
})

test_that("Disaggregation returns tibble", {
  
  test4 <- disc_mit(
    postsynth = postsynth,
    data = data3, 
    holdout_data = data3,
    summary = FALSE
  )
  
  expect_s3_class(test4$results, "tbl")
  expect_identical(names(test4$results),
                   c("source","a", "b", "distance", 
                     "pseudo_probability", "prediction"))
  
  expect_s3_class(test4$roc, "tbl")
  expect_identical(names(test4$roc),
                   c(".threshold", "specificity", "sensitivity"))
  
})

test_that("disc_mit() input errors ", {
  
  expect_error(
    disc_mit(
      postsynth = postsynth,
      data = data3, 
      holdout_data = data3,
      threshold_percentile = "abc"
    )
  )
  
  expect_error(
    disc_mit(
      postsynth = postsynth,
      data = data3, 
      holdout_data = data3,
      threshold_percentile = -1
    )
  )
  
  expect_error(
    disc_mit(
      postsynth = postsynth,
      data = data3, 
      holdout_data = data3,
      threshold_percentile = 1.1
    )
  )

  
})





