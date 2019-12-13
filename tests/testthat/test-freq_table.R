library(dplyr)
library(freqtables)

data(mtcars)

context("test-freq_table.R")

# =============================================================================
# Test one-way frequency tables
# =============================================================================
df <- mtcars %>%
  group_by(am) %>%
  freq_table()

test_that("Dimensions of the object returned by freq_table are as expected", {
  rows    <- nrow(df)
  columns <- ncol(df)

  expect_equal(rows, 2L)
  expect_equal(columns, 7L)
})

test_that("Class of freq_table_one_way is freq_table_one_way", {
  expect_is(df, "freq_table_one_way")
})

test_that("The correct var name is returned by freq_table", {
  name <- names(df)[1]
  expect_match(name, "var")
})

test_that("The correct cat var name is returned by freq_table", {
  name <- names(df)[2]
  expect_match(name, "cat")
})

test_that("The correct variables levels are returned by freq_table", {
  levels <- pull(df, cat)
  expect_equal(levels, c(0, 1))
})

test_that("The correct default statistics are returned by freq_table", {
  n        <- pull(df, n)
  n_total  <- pull(df, n_total)
  percent  <- pull(df, percent)
  lcl      <- pull(df, lcl)
  ucl      <- pull(df, ucl)

  expect_equal(n,       c(19, 13))
  expect_equal(n_total, c(32, 32))
  expect_equal(percent, c(59.38, 40.62))
  expect_equal(lcl,     c(40.94, 24.50))
  expect_equal(ucl,     c(75.50, 59.06))
})

# Testing Wald CI's
# -----------------
df <- mtcars %>%
  group_by(am) %>%
  freq_table(ci_type = "wald")

test_that("The correct Wald CI's are returned by freq_table", {
  lcl <- pull(df, lcl)
  ucl <- pull(df, ucl)

  expect_equal(lcl, c(41.38, 22.63))
  expect_equal(ucl, c(77.37, 58.62))
})




# =============================================================================
# Test two-way freq tables
# =============================================================================
df <- mtcars %>%
  group_by(am, cyl) %>%
  freq_table()

test_that("Dimensions of the object returned by freq_table are as expected", {
  rows    <- nrow(df)
  columns <- ncol(df)

  expect_equal(rows, 6L)
  expect_equal(columns, 10L)
})

test_that("Class of freq_table_two_way is freq_table_two_way", {
  expect_is(df, "freq_table_two_way")
})

test_that("The correct var names are returned by freq_table", {
  row_var <- pull(df, row_var)
  col_var <- pull(df, col_var)

  expect_match(row_var, "am")
  expect_match(col_var, "cyl")
})

test_that("The correct variables levels are returned by freq_table", {
  row_cat <- pull(df, row_cat)
  col_cat <- pull(df, col_cat)

  expect_equal(row_cat, c(0, 0, 0, 1, 1, 1))
  expect_equal(col_cat, c(4, 6, 8, 4, 6, 8))
})

test_that("The correct default statistics are returned by freq_table", {
  n           <- pull(df, n)
  n_row       <- pull(df, n_row)
  n_total     <- pull(df, n_total)
  percent_row <- pull(df, percent_row)
  lcl_row     <- pull(df, lcl_row)
  ucl_row     <- pull(df, ucl_row)

  expect_equal(n,           c(3, 4, 12, 8, 3, 2))
  expect_equal(n_row,       c(rep(19, 3), rep(13, 3)))
  expect_equal(n_total,     c(rep(32, 6)))
  expect_equal(percent_row, c(15.79, 21.05, 63.16, 61.54, 23.08, 15.38))
  expect_equal(lcl_row,     c(4.78, 7.58, 38.76, 32.30, 6.91, 3.43))
  expect_equal(ucl_row,     c(41.20, 46.44, 82.28, 84.29, 54.82, 48.18))
})

# Checking overall percents and CI's
# ----------------------------------
df <- mtcars %>%
  group_by(am, cyl) %>%
  freq_table(output = "all")

test_that("The correct overall percents and 95% CI's are returned", {
  percent_total <- pull(df, percent_total)
  lcl_total     <- pull(df, lcl_total)
  ucl_total     <- pull(df, ucl_total)

  expect_equal(percent_total, c(9.38, 12.50, 37.50, 25.00, 9.38, 6.25))
  expect_equal(lcl_total,     c(2.86, 4.51, 21.97, 12.51, 2.86, 1.45))
  expect_equal(ucl_total,     c(26.66, 30.19, 56.11, 43.72, 26.66, 23.24))
})




# =============================================================================
# Check changing default parameters
# =============================================================================

# 99% confidence intervals instead of 95% confidence intervals
# ------------------------------------------------------------
alpha <- 1 - .99
t <- 1 - alpha / 2

df <- mtcars %>%
  group_by(am) %>%
  freq_table(t_prob = t)

test_that("The 99% confidence intervals are correct", {
  lcl <- pull(df, lcl)
  ucl <- pull(df, ucl)

  expect_equal(lcl, c(34.89, 20.05))
  expect_equal(ucl, c(79.95, 65.11))
})

# digits = 3
# ----------
df <- mtcars %>%
  group_by(am) %>%
  freq_table(digits = 3)

test_that("The 'digits' parameter works as expected", {
  percent <- pull(df, percent)
  lcl     <- pull(df, lcl)
  ucl     <- pull(df, ucl)

  expect_equal(percent, c(59.375, 40.625))
  expect_equal(lcl,     c(40.942, 24.502))
  expect_equal(ucl,     c(75.498, 59.058))
})




# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df, alpha, t)
detach("package:dplyr", unload=TRUE)
detach("package:freqtables", unload=TRUE)
