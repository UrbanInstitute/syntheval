# confidential data
df1 <- list(
  synthetic_data = data.frame(
    a = c("apple", "apple", "banana", "banana"),
    b = c("apple", "apple", "banana", "banana"),
    c = c("apple", "apple", "banana", "banana")
  )
)

class(df1) <- "postsynth"

df2 <- data.frame(
  a = c("cantaloupe"),
  b = c("cantaloupe"),
  c = c("cantaloupe")
)



kmarginals(df1, df2, k = 3)

test_that("kmarginals is 1000 for exact matches ", {
  
  expect_equal(kmarginals(df1, df1$synthetic_data, k = 1), 1000)
  expect_equal(kmarginals(df1, df1$synthetic_data, k = 2), 1000)
  expect_equal(kmarginals(df1, df1$synthetic_data, k = 3), 1000)
  
})
