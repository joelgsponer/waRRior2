context("test-snippets")
library(waRRior)
library(stringr)

test_that("80 Chr line", {
  expect_equal(80, stringr::str_length(waRRior::snippet_full_line_title("a","a",0)))
})
