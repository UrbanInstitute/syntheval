# test with postsynth
test_that("lm overlap is 1 for identical data ", {
  
  overlap1 <- util_ci_overlap(eval_data(cars, cars), formula = dist ~ speed)
  
  expect_equal(overlap1$ci_overlap$overlap, c(1, 1))
  expect_equal(overlap1$ci_overlap$coef_diff, c(0, 0))
  expect_equal(overlap1$ci_overlap$sign_match, c(TRUE, TRUE))
  expect_equal(overlap1$ci_overlap$significance_match, c(TRUE, TRUE))
  expect_equal(overlap1$ci_overlap$ss_match, c(TRUE, TRUE))
  expect_equal(overlap1$ci_overlap$sso_match, c(TRUE, TRUE))
  
  original1 <- overlap1$coefficient %>%
    dplyr::filter(source == "original") %>%
    dplyr::select(-source)
  
  synthetic1 <- overlap1$coefficient %>%
    dplyr::filter(source == "synthetic") %>%
    dplyr::select(-source)
  
  expect_equal(original1, synthetic1)
  
})

# test with postsynth
test_that("lm overlap is 0 for adjacent data ", {
  
  # shift the outcome variable by twice the standard error for the intercept
  # this should create an adjacent confidence interval
  std_error <- broom::tidy(lm(dist ~ speed, data = cars)) %>%
    dplyr::filter(term == "(Intercept)") %>%
    dplyr::pull(std.error)
  
  offset <- std_error * qt(p = 0.975, df = 48) * 2
  
  cars2 <- cars
  cars2$dist <- cars$dist + offset
  
  eval_data <- eval_data(conf_data = cars2, synth_data = cars)
  
  overlap2 <- util_ci_overlap(eval_data, formula = dist ~ speed)
  
  expect_equal(overlap2$ci_overlap$overlap, c(0, 1))
  
})

test_that("compute_ci_overlap works with pre-fit lm models ", {
  
  original_model <- lm(dist ~ speed, data = cars)
  synthetic_model <- lm(dist ~ speed, data = cars)
  
  overlap1 <- compute_ci_overlap(original_model, synthetic_model)
  
  expect_equal(overlap1$ci_overlap$overlap, c(1, 1))
  expect_equal(overlap1$ci_overlap$coef_diff, c(0, 0))
  expect_equal(overlap1$ci_overlap$sign_match, c(TRUE, TRUE))
  expect_equal(overlap1$ci_overlap$significance_match, c(TRUE, TRUE))
  expect_equal(overlap1$ci_overlap$ss_match, c(TRUE, TRUE))
  expect_equal(overlap1$ci_overlap$sso_match, c(TRUE, TRUE))
  
  original1 <- overlap1$coefficient %>%
    dplyr::filter(source == "original") %>%
    dplyr::select(-source)
  
  synthetic1 <- overlap1$coefficient %>%
    dplyr::filter(source == "synthetic") %>%
    dplyr::select(-source)
  
  expect_equal(original1, synthetic1)
  
})

# test with postsynth
test_that("binomial overlap is 1 for identical data ", {

  cars_binomial <- cars
  cars_binomial$crash <- as.numeric(cars_binomial$dist > 22)
  
  overlap_binomial <- util_ci_overlap(
    eval_data(cars_binomial, cars_binomial), 
    formula = crash ~ speed,
    family = binomial
  )
  
  expect_equal(overlap_binomial$ci_overlap$overlap, c(1, 1))
  expect_equal(overlap_binomial$ci_overlap$coef_diff, c(0, 0))
  expect_equal(overlap_binomial$ci_overlap$sign_match, c(TRUE, TRUE))
  expect_equal(overlap_binomial$ci_overlap$significance_match, c(TRUE, TRUE))
  expect_equal(overlap_binomial$ci_overlap$ss_match, c(TRUE, TRUE))
  expect_equal(overlap_binomial$ci_overlap$sso_match, c(TRUE, TRUE))
  
  original_binomial <- overlap_binomial$coefficient %>%
    dplyr::filter(source == "original") %>%
    dplyr::select(-source)
  
  synthetic_binomial <- overlap_binomial$coefficient %>%
    dplyr::filter(source == "synthetic") %>%
    dplyr::select(-source)
  
  expect_equal(original_binomial, synthetic_binomial)
  
})

# test with postsynth
test_that("binomial overlap is 0 for adjacent data ", {
  

  # simulate two data sets with adjacent CIs
  set.seed(123)
  
  n <- 20000
  
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  
  beta0 <- 0
  beta1 <- 0.20
  beta2 <- 0.2227161 # found with numerical optimization
  
  p1 <- plogis(beta0 + beta1 * x1)
  p2 <- plogis(beta0 + beta2 * x2)
  
  y1 <- rbinom(n, 1, p1)
  y2 <- rbinom(n, 1, p2)
  
  conf_data <- tibble::tibble(y = y1, x = x1)
  synth_data <- tibble::tibble(y = y2, x = x2)

  eval_data <- eval_data(conf_data = conf_data, synth_data = synth_data)
  
  overlap <- util_ci_overlap(eval_data, formula = y ~ x, family = binomial)
  
  expect_equal(round(overlap$ci_overlap$overlap[2], 3), 0)
  
})

test_that("poisson overlap is near 0 for adjacent data ", {
  
  # simulate data with adjacent confidence intervals
  set.seed(123)
  
  n <- 5000
  
  # Dataset 1
  x <- rnorm(n)
  
  beta0 <- log(5)
  beta1 <- 4
  beta2 <- 4.00174 # solved for numerically
  
  
  mu1 <- exp(beta0 + beta1 * x)
  mu2 <- exp(beta0 + beta2 * x)
  
  y1 <- rpois(n, mu1)
  y2 <- rpois(n, mu2)
  
  synth_data <- tibble::tibble(y = y1, x = x)
  conf_data <- tibble::tibble(y = y2, x = x)
  
  eval_data <- eval_data(conf_data = conf_data, synth_data = synth_data)
  
  overlap <- util_ci_overlap(eval_data, formula = y ~ x, family = poisson)
  
  expect_equal(round(overlap$ci_overlap$overlap[2], 1), 0)
  
})

test_that("util_ci_overlap errors when a glm does not converge ", {
  
  # perfectly separated data causes glm.fit to fail to converge and the
  # subsequent profile likelihood confidence interval calculation to error
  x <- seq(-10, 10, length.out = 40)
  y <- as.numeric(x > 0)
  
  separated_data <- tibble::tibble(y = y, x = x)
  
  eval_data <- eval_data(conf_data = separated_data, synth_data = separated_data)
  
  expect_error(
    suppressWarnings(
      util_ci_overlap(eval_data, formula = y ~ x, family = binomial)
    ),
    regexp = "did not converge"
  )
  
})

