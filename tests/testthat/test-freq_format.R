library(dplyr)
library(freqtables)

data(mtcars)

testthat::context("test-freq_format.R")

# =============================================================================
# Test one-way frequency table
# =============================================================================
df <- mtcars %>%
  freq_table(am) %>%
  freq_format(
    recipe = "percent (lcl - ucl)",
    name = "percent_95",
    digits = 2
  )

testthat::test_that("Dimensions of the object returned by freq_format are as expected", {
  dims <- dim(df)
  testthat::expect_equal(dims, c(2L, 10L))
})

testthat::test_that("The the name argument to freq_format works as expected", {
  name <- names(df)[10]
  testthat::expect_match(name, "percent_95")
})

testthat::test_that("The correct statistics are returned by freq_format", {
  percent_95 <- pull(df, percent_95)
  testthat::expect_equal(percent_95, c("59.38 (40.94 - 75.50)", "40.62 (24.50 - 59.06)"))
})




# =============================================================================
# Test one-way frequency table, stats = n and percent
# =============================================================================
df <- mtcars %>%
  freq_table(am) %>%
  freq_format(
    recipe = "n (percent%)",
    name = "n_percent",
    digits = 2
  )

testthat::test_that("The correct statistics are returned by freq_format", {
  n_percent <- pull(df, n_percent)
  testthat::expect_equal(n_percent, c("19 (59.38%)", "13 (40.62%)"))
})




# =============================================================================
# Test two-way frequency table, stats = percent and ci (default)
# =============================================================================
df <- mtcars %>%
  freq_table(am, vs) %>%
  freq_format(
    recipe = "percent_row (lcl_row - ucl_row)",
    name = "percent_row_95",
    digits = 2
  )

testthat::test_that("Dimensions of the object returned by freq_format are as expected", {
  dims <- dim(df)
  testthat::expect_equal(dims, c(4L, 18L))
})

testthat::test_that("The correct statistics are returned by freq_format", {
  percent_row_95 <- pull(df, percent_row_95)
  testthat::expect_equal(percent_row_95, c(
    "63.16 (38.76 - 82.28)", "36.84 (17.72 - 61.24)", "46.15 (20.83 - 73.63)",
    "53.85 (26.37 - 79.17)"
    )
  )
})




# =============================================================================
# Test two-way frequency table, stats = n and percent
# =============================================================================
df <- mtcars %>%
  freq_table(am, vs) %>%
  freq_format(
    recipe = "n_row (percent_row)",
    name = "n_percent_row",
    digits = 2
  )

testthat::test_that("Dimensions of the object returned by freq_format are as expected", {
  dims <- dim(df)
  testthat::expect_equal(dims, c(4L, 18L))
})

testthat::test_that("The correct statistics are returned by freq_format", {
  n_percent_row <- pull(df, n_percent_row)
  testthat::expect_equal(
    n_percent_row,
    c("19 (63.16)", "19 (36.84)", "13 (46.15)", "13 (53.85)")
  )
})


# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df)
detach("package:dplyr", unload=TRUE)
detach("package:freqtables", unload=TRUE)
