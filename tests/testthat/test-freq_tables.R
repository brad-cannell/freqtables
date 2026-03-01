library(dplyr)
library(freqtables)

data(mtcars)

testthat::context("test-freq_tables.R")

# =============================================================================
# Test error checks
# =============================================================================
testthat::test_that("Error when a data frame isn't passed", {
  testthat::expect_error(
    freq_tables(1:10, am),
    "freq_tables expects a data frame"
  )
})

testthat::test_that("Error when no column names are passed", {
  testthat::expect_error(
    freq_tables(mtcars),
    "No column names were passed"
  )
})


# =============================================================================
# Test basic usage with multiple variables
# =============================================================================
df <- mtcars %>%
  freq_tables(am, cyl)

testthat::test_that("freq_tables returns a tibble with correct structure", {
  testthat::expect_equal(nrow(df), 2L)
  testthat::expect_true("col" %in% names(df))
  testthat::expect_true("freq_table" %in% names(df))
})

testthat::test_that("freq_tables returns correct variable names", {
  testthat::expect_equal(df$col, c("am", "cyl"))
})

testthat::test_that("freq_tables nested data frames are freq_table_one_way", {
  testthat::expect_is(df$freq_table[[1]], "freq_table_one_way")
  testthat::expect_is(df$freq_table[[2]], "freq_table_one_way")
})

testthat::test_that("freq_tables nested data frames have correct dimensions", {
  testthat::expect_equal(nrow(df$freq_table[[1]]), 2L) # am: 0, 1
  testthat::expect_equal(nrow(df$freq_table[[2]]), 3L) # cyl: 4, 6, 8
})


# =============================================================================
# Test with grouped data
# =============================================================================
df_grouped <- mtcars %>%
  group_by(vs) %>%
  freq_tables(am, cyl)

testthat::test_that("freq_tables works with grouped data", {
  testthat::expect_equal(nrow(df_grouped), 2L)
  testthat::expect_is(df_grouped$freq_table[[1]], "freq_table_two_way")
})


# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df, df_grouped)
detach("package:dplyr", unload = TRUE)
detach("package:freqtables", unload = TRUE)
