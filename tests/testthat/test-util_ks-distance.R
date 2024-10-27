df <- data.frame(
  a = c(NA, 1, 2, 3, 4),
  b = c(NA, 1, 2, 3, 4),
  c = c(NA, "a", "a", "b", "b")
)

test_that("KS is 0 ", {

  syn <- list(
    synthetic_data = data.frame(
      a = c(1, 2, 3, 4, NA),
      b = c(1, 2, 3, 4, NA),
      c = c("a", "a", "b", "b", NA)
    ),
    jth_synthesis_time = data.frame(
      variable = factor(c("a", "b"))
    )
  ) %>%
    structure(class = "postsynth")
  
  D <- util_ks_distance(postsynth = syn, data = df, na.rm = TRUE)
  
  expect_equal(D$D, rep(0, 8))
  
})

test_that("KS distance is 0.5 ", {

  syn <- list(
    synthetic_data = data.frame(
      a = c(3, 4, 5, 6, NA),
      b = c(3, 4, 5, 6, NA),
      c = c("a", "a", "b", "b", NA)
    ),
    jth_synthesis_time = data.frame(
      variable = factor(c("a", "b"))
    )
  ) %>%
    structure(class = "postsynth")
  
  D <- util_ks_distance(postsynth = syn, data = df, na.rm = TRUE)
  
  expect_equal(D$D, rep(0.5, 4))
  
})

test_that("KS distance is 1 ", {
  
  syn <- list(
    synthetic_data = data.frame(
      a = c(60, 70, 80, 90, NA),
      b = c(60, 70, 80, 90, NA),
      c = c("a", "a", "b", "b", NA)
    ),
    jth_synthesis_time = data.frame(
      variable = factor(c("a", "b"))
    )
  ) %>%
    structure(class = "postsynth")
  
  D <- util_ks_distance(postsynth = syn, data = df, na.rm = TRUE)
  
  expect_equal(D$D, c(1, 1))
  
})

test_that("KS distance works with NA ", {
  
  syn <- list(
    synthetic_data = acs_conf
  ) %>%
    structure(class = "postsynth")
  
  D <- util_ks_distance(
    postsynth = syn, 
    data = acs_conf,
    na.rm = TRUE)
  
  expect_equal(max(D$D), 0)
  
})
