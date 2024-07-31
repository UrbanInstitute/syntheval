

test_that("eval_data input errors", {
  
  expect_error(
    eval_data(
      conf_data = "not a dataframe", 
      synth_data = mtcars
    )
  )
  
  expect_error(
    eval_data(
      conf_data = mtcars, 
      synth_data = "not a dataframe"
    )
  )
  
  expect_error(
    eval_data(
      conf_data = mtcars, 
      synth_data = mtcars, 
      holdout_data = "not a dataframe"
    )
  )
  
  expect_error(
    eval_data(
      conf_data = mtcars, 
      synth_data = list(mtcars, "not a dataframe")
    )
  )
   
})

test_that("eval_data holdout and confidential data columns agree", {
  
  expect_error(
    eval_data(
      conf_data = mtcars,
      synth_data = mtcars,
      holdout_data = data.frame(a=1:5)
    )
  )
})

test_that("eval_data calculates n_rep", {
  
  rep1_ed <- eval_data(
    conf_data = mtcars,
    synth_data = mtcars
  )
  
  expect_equal(rep1_ed$n_rep, 1)
  
  rep3_ed <- eval_data(
    conf_data = mtcars,
    synth_data = list(mtcars, mtcars, mtcars)
  )
  
  expect_equal(rep3_ed$n_rep, 3)
  
})