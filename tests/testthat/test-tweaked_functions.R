context("test-tweaked_functions")
library(waRRior)

test_that("no NA no -Inf max mean min", {
  expect_equal(2,       max_noNA_noInf(c(1,2)))
  expect_equal(2,       max_noNA_noInf(c(NA,2)))
  expect_equal(T, is.na(max_noNA_noInf(c(NA,NA))))
  expect_equal(1.5,     mean_noNA_noInf(c(1,2)))
  expect_equal(2,       mean_noNA_noInf(c(NA,2)))
  expect_equal(T, is.na(mean_noNA_noInf(c(NA,NA))))
  expect_equal(1,       min_noNA_noInf(c(1,2)))
  expect_equal(2,       min_noNA_noInf(c(NA,2)))
  expect_equal(T, is.na(min_noNA_noInf(c(NA,NA))))
})
