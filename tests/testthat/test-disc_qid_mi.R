acs_eval <- eval_data(
  conf_data = acs_conf,
  synth_data = acs_lr_synths
)

acs_eval_holdout <- eval_data(
  conf_data = acs_conf[sample(nrow(acs_conf)), ],
  synth_data = acs_lr_synths,
  holdout_data = acs_holdout[sample(nrow(acs_holdout)), ]
)

qid_keys <- c("county", "gq", "sex", "empstat", "classwkr")

acs_eval_agg <- aggregate_qid_eval(
  eval_data = acs_eval, 
  keys = qid_keys
)
acs_eval_holdout_agg <- aggregate_qid_eval(
  eval_data = acs_eval_holdout, 
  keys = qid_keys
)

test_that("aggregate_qid_eval expected errors", {
  
  # wrong input class
  expect_error(
    aggregate_qid_eval("not_eval_data", c("a", "b"))
  )
  
  # wrong n_rep
  expect_error(
    aggregate_qid_eval(
      eval_data(
        conf_data = acs_conf,
        synth_data = acs_conf
      )
    )
  )
  
})

test_that("aggregate_qid_eval expected behavior", {
  
  # expect number of rows is consistent
  # first, calculate expected number of rows, which should be the product of...
  expected_rows <- acs_eval[["n_rep"]] * # the number of synthetic data reps...
    purrr::reduce(
      # ...times the number of levels in each factor...
      purrr::map(
        .x = qid_keys, 
        .f = \(x) {length(levels(acs_conf[[x]]))}
      ), 
    `*`) # all multiplied together
    
  # check that this equals the size of the computed result
  expect_equal(nrow(acs_eval_agg), expected_rows)
  expect_equal(nrow(acs_eval_holdout_agg), expected_rows)
  
  # expect key_id definitions are consistent
  expect_identical(
    acs_eval_agg %>% 
      dplyr::select(
        dplyr::all_of(c("key_id", qid_keys))
      ),
    acs_eval_holdout_agg %>% 
      dplyr::select(
        dplyr::all_of(c("key_id", qid_keys))
      )
  )

})

test_that("plot_prob_qid_partition correct ggplot2 objects", {
  
  p1 <- plot_prob_qid_partition(
    agg_eval_data = acs_eval_agg, 
    keys = qid_keys,
    max_k = 5
  )
  
  # expect plot object
  expect_true("ggplot" %in% class(p1))
  
  # expect prob data calculated properly
  expect_identical(
    names(p1$data), 
    c(qid_keys, "raw_n_conf", "s_synth", ".group")
  )
  
  expect_true(
    all(p1$data[["s_synth"]] %in% (seq(0, 30) / 30.))
  )
  
  expect_true(
    all(p1$data[["raw_n_conf"]] <= 5)
  )
  
})

test_that("plot_prob_qid_abs_err returns correct ggplot2 objects", {
  
  p2 <- plot_prob_qid_abs_err(
    agg_eval_data = acs_eval_agg, 
    keys = qid_keys,
    max_k = 5
  )
  
  # expect plot object
  expect_true("ggplot" %in% class(p2))
  
  # expect prob data calculated properly
  expect_true(
    all(p2$data[["qtile"]] %in% c("50%ile", "75%ile", "90%ile"))
  )
  
  expect_true(
    all(p2$data[["raw_n_conf"]] <= 5)
  )
  
})