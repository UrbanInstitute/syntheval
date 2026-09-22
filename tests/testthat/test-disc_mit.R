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
    summary = FALSE
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

test_that("chunked distances are equivalent to unchunked distances (single replicate)", {
  
  set.seed(20240603)
  
  conf <- tibble::tibble(
    num = rnorm(30),
    cat = sample(c("a", "b", "c"), 30, replace = TRUE)
  )
  
  holdout <- tibble::tibble(
    num = rnorm(30),
    cat = sample(c("a", "b", "c"), 30, replace = TRUE)
  )
  
  synth <- tibble::tibble(
    num = rnorm(18),
    cat = sample(c("a", "b", "c"), 18, replace = TRUE)
  )
  
  ed <- eval_data(conf_data = conf, synth_data = synth, holdout_data = holdout)
  
  unchunked <- disc_mit(ed, summary = FALSE)
  chunked <- disc_mit(ed, summary = FALSE, chunk_size = 7)
  
  expect_equal(chunked$results$distance, unchunked$results$distance)
  expect_equal(chunked$results$prediction, unchunked$results$prediction)
  expect_equal(chunked$roc, unchunked$roc)
  
})

test_that("chunked distances are equivalent to unchunked distances (multiple replicates)", {
  
  set.seed(20240604)
  
  conf <- tibble::tibble(
    num = rnorm(20),
    cat = sample(c("a", "b"), 20, replace = TRUE)
  )
  
  holdout <- tibble::tibble(
    num = rnorm(20),
    cat = sample(c("a", "b"), 20, replace = TRUE)
  )
  
  synths <- lapply(1:3, \(i) {
    tibble::tibble(
      num = rnorm(12),
      cat = sample(c("a", "b"), 12, replace = TRUE)
    )
  })
  
  ed <- eval_data(conf_data = conf, synth_data = synths, holdout_data = holdout)
  
  unchunked <- disc_mit(ed, summary = FALSE)
  chunked <- disc_mit(ed, summary = FALSE, chunk_size = 9)
  
  expect_equal(chunked$results$distance, unchunked$results$distance)
  expect_equal(chunked$results$prediction, unchunked$results$prediction)
  
})

test_that("explicit variables equal to all common columns match default behavior", {
  
  ed <- eval_data(
    conf_data = data3,
    synth_data = postsynth,
    holdout_data = data3
  )
  
  default_result <- disc_mit(ed, summary = FALSE)
  explicit_result <- disc_mit(ed, summary = FALSE, variables = c("a", "b"))
  
  expect_equal(explicit_result$results$distance, default_result$results$distance)
  
})

test_that("variable subset changes the metric but preserves output shape", {
  
  ed <- eval_data(
    conf_data = data3,
    synth_data = postsynth,
    holdout_data = data3
  )
  
  full_result <- disc_mit(ed, summary = FALSE)
  subset_result <- disc_mit(ed, summary = FALSE, variables = "a")
  
  expect_equal(nrow(subset_result$results), nrow(full_result$results))
  
})

test_that("variables not present in the data raise an error", {
  
  ed <- eval_data(
    conf_data = data3,
    synth_data = postsynth,
    holdout_data = data3
  )
  
  expect_error(
    disc_mit(ed, variables = "not_a_column")
  )
  
})

test_that("blocked_variables restricts comparisons to exact matches", {
  
  set.seed(20240607)
  
  # "grp" only reproduces exactly for "a"; "b" is shifted far away, so if
  # blocking is respected these records can never appear to match "a" records
  conf <- tibble::tibble(
    grp = rep(c("a", "b"), each = 10),
    num = rnorm(20)
  )
  
  holdout <- tibble::tibble(
    grp = rep(c("a", "b"), each = 10),
    num = rnorm(20)
  )
  
  synth <- tibble::tibble(
    grp = rep(c("a", "b"), each = 10),
    num = c(conf$num[1:10], rnorm(10, mean = 100))
  )
  
  ed <- eval_data(conf_data = conf, synth_data = synth, holdout_data = holdout)
  
  result <- disc_mit(ed, summary = FALSE, blocked_variables = "grp")
  
  # every "a" conf record has an exact "a" match, so its distance is 0
  expect_true(all(result$results$distance[result$results$grp == "a" & result$results$source == "training"] == 0))
  
  # "b" records can never be near 0 since the matching "b" synthetic block is shifted away
  expect_true(all(result$results$distance[result$results$grp == "b"] > 0))
  
})

test_that("blocked_variables assigns the maximum distance when a block has no synthetic match", {
  
  conf <- tibble::tibble(grp = "c", num = 1)
  holdout <- tibble::tibble(grp = "c", num = 2)
  synth <- tibble::tibble(grp = "d", num = 1)
  
  ed <- eval_data(conf_data = conf, synth_data = synth, holdout_data = holdout)
  
  result <- disc_mit(ed, summary = FALSE, blocked_variables = "grp")
  
  expect_equal(result$results$distance, c(1, 1))
  
})

test_that("blocked_variables with a single shared block matches unblocked results", {
  
  # "grp" is constant across every row, so blocking on it puts every record
  # in the same single block, which shouldn't change which records are compared
  conf <- tibble::tibble(grp = "x", a = c(1, 1, 2, 2), b = c(1, 1, 2, 2))
  holdout <- conf
  synth <- tibble::tibble(grp = "x", a = c(1, 1, 1, 1), b = c(1, 1, 1, 1))
  
  ed <- eval_data(conf_data = conf, synth_data = synth, holdout_data = holdout)
  
  unblocked_result <- disc_mit(ed, summary = FALSE, variables = c("a", "b"))
  blocked_result <- disc_mit(ed, summary = FALSE, variables = c("a", "b"), blocked_variables = "grp")
  
  expect_equal(blocked_result$results$distance, unblocked_result$results$distance)
  
})

test_that("blocked_variables works across multiple synthetic replicates", {
  
  # gower_topn() warns about zero-range columns within these tiny fixture
  # blocks; that's expected here and doesn't affect the shape being tested
  result <- suppressWarnings(
    disc_mit(
      eval_data(
        conf_data = data3,
        synth_data = list(data1, data1, data1),
        holdout_data = data3
      ),
      summary = FALSE,
      blocked_variables = "a"
    )
  )
  
  expect_s3_class(result$results, "data.frame")
  expect_equal(dim(result$results)[1], 24)
  
})

test_that("blocked_variables not present in the data raise an error", {
  
  ed <- eval_data(
    conf_data = data3,
    synth_data = postsynth,
    holdout_data = data3
  )
  
  expect_error(
    disc_mit(ed, blocked_variables = "not_a_column")
  )
  
})

test_that("sample_prop subsamples blended data while preserving source proportions", {
  
  set.seed(20240608)
  
  conf <- tibble::tibble(num = rnorm(20))
  holdout <- tibble::tibble(num = rnorm(20))
  synth <- tibble::tibble(num = rnorm(20))
  
  ed <- eval_data(conf_data = conf, synth_data = synth, holdout_data = holdout)
  
  result <- disc_mit(ed, summary = FALSE, sample_prop = 0.5, sampling_seed = 42)
  
  expect_equal(as.integer(table(result$results$source)), c(10, 10))
  
})

test_that("sample_prop is reproducible given the same sampling_seed", {
  
  conf <- tibble::tibble(num = rnorm(20))
  holdout <- tibble::tibble(num = rnorm(20))
  synth <- tibble::tibble(num = rnorm(20))
  
  ed <- eval_data(conf_data = conf, synth_data = synth, holdout_data = holdout)
  
  result1 <- disc_mit(ed, summary = FALSE, sample_prop = 0.5, sampling_seed = 123)
  result2 <- disc_mit(ed, summary = FALSE, sample_prop = 0.5, sampling_seed = 123)
  
  expect_equal(result1$results, result2$results)
  
})

test_that("sample_prop outside of (0, 1] raises an error", {
  
  ed <- eval_data(
    conf_data = data3,
    synth_data = postsynth,
    holdout_data = data3
  )
  
  expect_error(
    disc_mit(ed, sample_prop = 0)
  )
  
  expect_error(
    disc_mit(ed, sample_prop = 1.5)
  )
  
})

test_that("sampling_strata not present in the data raise an error", {
  
  ed <- eval_data(
    conf_data = data3,
    synth_data = postsynth,
    holdout_data = data3
  )
  
  expect_error(
    disc_mit(ed, sampling_strata = "not_a_column", sample_prop = 0.5)
  )
  
})
