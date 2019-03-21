context("test-conversion")

library(waRRior)
testthat::test_that("Function should return reversed factor levels",
                    {
                      a <- factor(seq(1,10))
                      b <- waRRior::reverse_factor_levels(a)
                        expect_equal(TRUE, all(levels(b) == rev(levels(a))))
                    })
