library(testthat)
library(senseR)

# Sample data
set.seed(42)
df_test <- data.frame(
  target = rnorm(50, mean = 100, sd = 10),
  proxy1 = rnorm(50, mean = 50, sd = 5),
  proxy2 = rnorm(50, mean = 60, sd = 8),
  proxy3 = rnorm(50, mean = 70, sd = 12)
)

test_that("senser() handles input errors", {
  expect_error(senser("not_a_df", proxy = "proxy1", target = "target"),
               "data must be a data.frame")
  expect_error(senser(df_test, proxy = c("proxyX"), target = "target"),
               "one or more proxy variables not found in data")
  expect_error(senser(df_test, proxy = "proxy1", target = "nonexistent"),
               "target variable not found in data")
  df_nonnum <- df_test
  df_nonnum$target <- as.character(df_nonnum$target)
  expect_error(senser(df_nonnum, proxy = "proxy1", target = "target"),
               "target variable must be numeric")
})

test_that("senser() runs and prints output for valid input", {
  out <- capture.output(
    senser(df_test, proxy = c("proxy1", "proxy2"), target = "target")
  )
  expect_true(length(out) > 0)
  expect_true(any(grepl("Proxy :", out)))
  expect_true(any(grepl("Score :", out)))
  expect_true(any(grepl("Status:", out)))
})

test_that("senser() supports Indonesian language", {
  out_id <- capture.output(
    senser(df_test, proxy = c("proxy1"), target = "target", lang = "indonesia")
  )
  expect_true(any(grepl("Proxy :", out_id)))
  expect_true(any(grepl("Score :", out_id)))
  expect_true(any(grepl("Status:", out_id)))
})

test_that("senser() score is within [0,1]", {
  scores <- sapply(c("proxy1","proxy2"), function(p){
    out <- capture.output(
      senser(df_test, proxy = p, target = "target")
    )
    scr_line <- out[grep("Score :", out)]
    as.numeric(sub("Score : ", "", scr_line))
  })
  expect_true(all(scores >= 0 & scores <= 1))
})

test_that("senser() skips non-numeric proxies gracefully", {
  df_mixed <- df_test
  df_mixed$proxy_nonnum <- letters[1:50]
  out <- capture.output(
    senser(df_mixed, proxy = c("proxy1", "proxy_nonnum"), target = "target")
  )
  expect_true(length(out) > 0)  # output tetap ada tapi fungsi tidak crash
})
