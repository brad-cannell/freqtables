library(dplyr)
library(freqtables)

data(mtcars)

testthat::context("test-freq_format.R")

# =============================================================================
# Test one-way frequency table formatting
# =============================================================================
df <- mtcars %>%
  freq_table(am) %>%
  freq_format(
    recipe = "prop (lcl - ucl)",
    name = "prop_95",
    digits = 4
  )

testthat::test_that("freq_format adds a formatted column", {
  testthat::expect_true("prop_95" %in% names(df))
})

testthat::test_that("freq_format returns correct formatted values", {
  prop_95 <- pull(df, prop_95)
  testthat::expect_equal(prop_95, c("0.5938 (0.4094 - 0.7550)", "0.4062 (0.2450 - 0.5906)"))
})


# =============================================================================
# Test one-way with n and prop recipe
# =============================================================================
df2 <- mtcars %>%
  freq_table(am) %>%
  freq_format(
    recipe = "n (prop)",
    name = "n_prop",
    digits = 2
  )

testthat::test_that("n and prop recipe works correctly", {
  n_prop <- pull(df2, n_prop)
  testthat::expect_equal(n_prop, c("19 (0.59)", "13 (0.41)"))
})


# =============================================================================
# Test one-way with percent = TRUE
# =============================================================================
df3 <- mtcars %>%
  freq_table(am, percent = TRUE) %>%
  freq_format(
    recipe = "percent (lcl - ucl)",
    name = "percent_95",
    digits = 2
  )

testthat::test_that("freq_format works with percent output", {
  percent_95 <- pull(df3, percent_95)
  testthat::expect_equal(percent_95, c("59.38 (40.94 - 75.50)", "40.62 (24.50 - 59.06)"))
})


# =============================================================================
# Test grouped frequency table formatting
# =============================================================================
df4 <- mtcars %>%
  group_by(cyl) %>%
  freq_table(am) %>%
  freq_format(
    recipe = "prop_group (lcl_group - ucl_group)",
    name = "prop_95",
    digits = 3
  )

testthat::test_that("freq_format works with grouped freq_table output", {
  testthat::expect_true("prop_95" %in% names(df4))
  testthat::expect_equal(nrow(df4), 6L)
})


# =============================================================================
# Test default name
# =============================================================================
df5 <- mtcars %>%
  freq_table(am) %>%
  freq_format(recipe = "n", digits = 0)

testthat::test_that("freq_format uses 'formatted_stats' as default name", {
  testthat::expect_true("formatted_stats" %in% names(df5))
})


# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df, df2, df3, df4, df5)
detach("package:dplyr", unload = TRUE)
detach("package:freqtables", unload = TRUE)
