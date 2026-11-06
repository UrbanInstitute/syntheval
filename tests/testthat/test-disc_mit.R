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
    ),
  jth_synthesis_time = data.frame(
    variable = factor(c("a", "b"))
  )
)

class(postsynth) <- "postsynth"



test_that("Perfect training match and perfect holdout lack of match", {

  test1 <- disc_mit(
    eval_data(
      conf_data = data1, 
      synth_data = postsynth,
      holdout_data = data2
    )
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
    eval_data(
      conf_data = data2, 
      synth_data = postsynth,
      holdout_data = data1
    )
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
    eval_data(
      conf_data = data3, 
      synth_data = postsynth,
      holdout_data = data3
    )
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
    eval_data(
      conf_data = data3,
      synth_data = postsynth,
      holdout_data = data3
    ),
    keep_row_level = TRUE
  )
  
  expect_s3_class(test4$results, "tbl")
  expect_identical(names(test4$results),
                   c("source", "a", "b", "distance", 
                     "pseudo_probability", "prediction"))
  
  expect_s3_class(test4$roc, "tbl")
  expect_identical(names(test4$roc),
                   c(".threshold", "specificity", "sensitivity"))
  
})

test_that("disc_mit() multiple synthesis", {
  
  test5 <- disc_mit(
    eval_data(
      conf_data = data3,
      synth_data = list(data1, data1, data1),
      holdout_data = data3
    )
  )
  
  expect_equal(
    test5$precision, 
    0.5
  )
  
  expect_equal(
    test5$recall, 
    0.5
  )
  
  expect_equal(
    test5$auc, 
    0.5
  )
  
  test6 <- disc_mit(
    eval_data(
      conf_data = data3,
      synth_data = list(data1, data1, data1),
      holdout_data = data3
    ),
    keep_row_level = TRUE
  )
  
  expect_s3_class(test6$results, "data.frame")
  expect_equal(dim(test6$results)[1], 24)

})

test_that("disc_mit() input errors ", {
  
  expect_error(
    disc_mit(
      eval_data(
        conf_data = data3,
        synth_data = postsynth,
        holdout_data = data3
      ),
      threshold_percentile = "abc"
    )
  )
  
  expect_error(
    disc_mit(
      eval_data(
        conf_data = data3,
        synth_data = postsynth,
        holdout_data = data3
      ),
      threshold_percentile = -1
    )
  )
  
  expect_error(
    disc_mit(
      eval_data(
        conf_data = data3,
        synth_data = postsynth,
        holdout_data = data3
      ),
      threshold_percentile = 1.1
    )
  )

  
})

test_that("non-finite Gower distances produce finite tied membership scores", {
  result <- disc_mit(
    eval_data(
      conf_data = data1,
      synth_data = data1,
      holdout_data = data1
    ),
    keep_row_level = TRUE
  )

  expect_equal(result$results$distance, rep(1, 8))
  expect_true(all(is.finite(result$results$pseudo_probability)))
  expect_equal(result$auc, 0.5)
})

test_that("disc_mit() requires nonempty confidential and holdout data", {
  
  expect_error(
    disc_mit(
      eval_data(
        conf_data = data3[0, ],
        synth_data = postsynth,
        holdout_data = data3
      )
    ),
    "requires nonempty conf_data"
  )

  expect_error(
    disc_mit(
      eval_data(
        conf_data = data3,
        synth_data = postsynth,
        holdout_data = data3[0, ]
      )
    ),
    "requires nonempty holdout_data"
  )
})

test_that("blocked variables restrict comparisons and are excluded from Gower distance", {

  ed <- eval_data(
    conf_data = tibble::tibble(block = "A", value = 0),
    synth_data = tibble::tibble(block = c("A", "B"), value = c(10, 0)),
    holdout_data = tibble::tibble(block = "A", value = 10)
  )

  result <- disc_mit(ed, keep_row_level = TRUE, blocked_variables = "block")

  expect_equal(result$results$distance, c(1, 0))

})

test_that("blocked distances use the same numeric range across blocks", {

  ed <- eval_data(
    conf_data = tibble::tibble(block = c("A", "B"), value = c(0, 200)),
    synth_data = tibble::tibble(block = c("A", "B"), value = c(5, 205)),
    holdout_data = tibble::tibble(block = c("A", "B"), value = c(100, 210))
  )

  result <- disc_mit(ed, keep_row_level = TRUE, blocked_variables = "block")

  expect_equal(
    result$results$distance,
    c(5, 5, 95, 5) / 210
  )

})

test_that("records with no matching synthetic block get distance one", {

  ed <- eval_data(
    conf_data = tibble::tibble(block = "A", value = 1),
    synth_data = tibble::tibble(block = "B", value = 1),
    holdout_data = tibble::tibble(block = "A", value = 2)
  )

  result <- disc_mit(ed, keep_row_level = TRUE, blocked_variables = "block")

  expect_equal(result$results$distance, c(1, 1))

})

test_that("missing block values do not match the literal string NA", {

  ed <- eval_data(
    conf_data = tibble::tibble(block = NA_character_, value = 0),
    synth_data = tibble::tibble(
      block = c(NA_character_, "NA"),
      value = c(0, 4)
    ),
    holdout_data = tibble::tibble(block = "NA", value = 0)
  )

  result <- disc_mit(ed, keep_row_level = TRUE, blocked_variables = "block")

  expect_equal(result$results$distance, c(0, 1))

})

test_that("blocking all variables gives exact-match distances", {

  ed <- eval_data(
    conf_data = tibble::tibble(block = "A"),
    synth_data = tibble::tibble(block = "A"),
    holdout_data = tibble::tibble(block = "B")
  )

  result <- disc_mit(ed, keep_row_level = TRUE, blocked_variables = "block")

  expect_equal(result$results$distance, c(0, 1))

})

test_that("blocking on a constant variable matches no blocking", {

  set.seed(20260603)

  make_data <- function(n) {
    tibble::tibble(
      num = rnorm(n),
      cat = sample(c("a", "b", "c"), n, replace = TRUE)
    )
  }

  conf <- make_data(20)
  holdout <- make_data(20)
  synth <- make_data(15)

  unblocked <- disc_mit(
    eval_data(conf_data = conf, synth_data = synth, holdout_data = holdout),
    keep_row_level = TRUE
  )

  # a constant block puts every record in one block, so nothing is excluded from
  # comparison; block is left out of Gower, so the unblocked run omits it too
  blocked <- disc_mit(
    eval_data(
      conf_data = dplyr::mutate(conf, block = "x"),
      synth_data = dplyr::mutate(synth, block = "x"),
      holdout_data = dplyr::mutate(holdout, block = "x")
    ),
    keep_row_level = TRUE,
    blocked_variables = "block"
  )

  expect_equal(blocked$results$distance, unblocked$results$distance)

})





