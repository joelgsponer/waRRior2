context("test-tma")
library(waRRior)

test_that("Tidy a table with crooked missing values", {
  fruits <- c("Pear", "Apple", "n.a", "X")
  veggies <- c("Cauliflower", "X", "Carrot", "n.a")
  d <- data.frame(veggies,fruits)
  d <- waRRior::tidy_missing_values(d, c("n.a", "X"), c("veggies", "fruits"))
  expect_equal(F, all(c("n.a", "X") %in% d$veggies, c("n.a", "X") %in% d$fruits))
})

test_that("Tidy column names with crooked characters", {
  fruits <- c("Pear", "Apple", "n.a", "X")
  veggies <- c("Cauliflower", "X", "Carrot", "n.a")
  d <- data.frame(veggies,fruits)
  colnames(d) <- c("() ", "//-")
  d <- waRRior::tidy_column_names(d)
  expect_equal(c("__", "__"), colnames(d))
})

test_that("Remove unwanted character in table", {
  fruits <- c("Pear", "App%le")
  veggies <- c("Cauliflower", "Carrot")
  d <- data.frame(veggies,fruits)
  d <- waRRior::tidy_unwanted_characters(d,value = "%",colums = colnames(d))
  expect_equal(T, all(!grepl(pattern = "%",x = d)))
})

