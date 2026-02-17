library(testthat)
library(senseR)

set.seed(42)
df_test <- data.frame(
  target = rnorm(50, mean = 100, sd = 10),
  proxy1 = rnorm(50, mean = 50, sd = 5),
  proxy2 = rnorm(50, mean = 60, sd = 8),
  proxy3 = rnorm(50, mean = 70, sd = 12)
)

test_that("senser() handles input errors", {

  expect_error(
    senser("not_a_df", proxy = "proxy1", target = "target"),
    regexp = "data",
    ignore.case = TRUE
  )

  expect_error(
    senser(df_test, proxy = "proxyX", target = "target"),
    regexp = "proxy",
    ignore.case = TRUE
  )

  expect_error(
    senser(df_test, proxy = "proxy1", target = "nonexistent"),
    regexp = "target",
    ignore.case = TRUE
  )

  df_nonnum <- df_test
  df_nonnum$target <- as.character(df_nonnum$target)

  expect_error(
    senser(df_nonnum, proxy = "proxy1", target = "target"),
    regexp = "numeric",
    ignore.case = TRUE
  )
})
