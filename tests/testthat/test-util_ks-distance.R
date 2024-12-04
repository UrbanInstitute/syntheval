df <- data.frame(
  a = c(NA, 1, 2, 3, 4),
  b = c(NA, 1, 2, 3, 4),
  c = c(NA, "a", "a", "b", "b")
)

test_that("KS is 0 ", {

  syn <- data.frame(
    a = c(1, 2, 3, 4, NA),
    b = c(1, 2, 3, 4, NA),
    c = c("a", "a", "b", "b", NA)
  )
  
  ed <- eval_data(conf_data = df, synth_data = syn)
  
  D <- util_ks_distance(ed, na.rm = TRUE)
  
  expect_equal(D$D, rep(0, 8))
  
})

test_that("KS distance is 0.5 ", {

  syn <- data.frame(
    a = c(3, 4, 5, 6, NA),
    b = c(3, 4, 5, 6, NA),
    c = c("a", "a", "b", "b", NA)
  )
  
  ed <- eval_data(conf_data = df, synth_data = syn)
  
  D <- util_ks_distance(ed, na.rm = TRUE)
  
  expect_equal(D$D, rep(0.5, 4))
  
})

test_that("KS distance is 1 ", {
  
  syn <- data.frame(
    a = c(60, 70, 80, 90, NA),
    b = c(60, 70, 80, 90, NA),
    c = c("a", "a", "b", "b", NA)
  )
  
  ed <- eval_data(conf_data = df, synth_data = syn)
  
  D <- util_ks_distance(ed, na.rm = TRUE)
  
  expect_equal(D$D, c(1, 1))
  
})

test_that("KS distance works with NA ", {
  
  ed <- eval_data(conf_data = acs_conf, synth_data = acs_conf)
  
  D <- util_ks_distance(ed, na.rm = TRUE)
  
  expect_equal(max(D$D), 0)
  
})
