# test with postsynth
test_that("overlap is zero for identical data ", {
  
  overlap1 <- regression_ci_overlap(postsynth = cars, data = cars, formula = dist ~ speed)
  
  expect_equal(overlap1$ci_overlap$overlap, c(1, 1))
  expect_equal(overlap1$ci_overlap$coef_diff, c(0, 0))
  expect_equal(overlap1$ci_overlap$sign_match, c(TRUE, TRUE))
  expect_equal(overlap1$ci_overlap$significance_match, c(TRUE, TRUE))
  expect_equal(overlap1$ci_overlap$ss_match, c(TRUE, TRUE))
  expect_equal(overlap1$ci_overlap$sso_match, c(TRUE, TRUE))
  
  original1 <- overlap1$coefficient |>
    dplyr::filter(source == "original") |>
    dplyr::select(-source)
  
  synthetic1 <- overlap1$coefficient |>
    dplyr::filter(source == "synthetic") |>
    dplyr::select(-source)
  
  expect_equal(original1, synthetic1)
  
})

# test with postsynth
test_that("overlap is 1 for adjacent data ", {
  
  # shift the outcome variable by twice the standard error for the intercept
  # this should create an adjacent confidence interval
  std_error <- broom::tidy(lm(dist ~ speed, data = cars)) |>
    dplyr::filter(term == "(Intercept)") |>
    dplyr::pull(std.error)
  
  offset <- std_error * qt(p = 0.975, df = 48) * 2
  
  cars2 <- cars
  cars2$dist <- cars$dist + offset
  
  overlap2 <- regression_ci_overlap(postsynth = cars, data = cars2, formula = dist ~ speed)
  
  expect_equal(overlap2$ci_overlap$overlap, c(0, 1))
  
})
