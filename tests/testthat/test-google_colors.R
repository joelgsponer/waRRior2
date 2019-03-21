context("test-google_colors")
library(waRRior)

test_that("Load google colors and check one color", {
  g <- google_colors_load(verbose = F)
  testthat::expect_equal("#F44336", g$Red$main)
})

test_that("Load google colors and check one accent color", {
  g <- google_colors_load(verbose = F)
  g_a <- google_colors_accents(level = 1)
  testthat::expect_equal("#FF8A80", g_a[1])
})
