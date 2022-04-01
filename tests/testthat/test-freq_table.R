library(dplyr)
library(freqtables)

data(mtcars)

testthat::context("test-freq_table.R")

# =============================================================================
# Test error checks
# - Error when a data frame isn't passed to the .data argument.
# - Error when more than 2 column names are passed to the ... argument.
# - Error when no column names are passed to the ... argument.

# testthat evaluates the error string with a regular expression remember to
# escape special characters like ( with a double back slash.
# =============================================================================
testthat::test_that("Error when a data frame isn't passed to the .data argument.", {
  vector <- 1:10
  testthat::expect_error(
    freq_table(1:10),
    "freq_table expects the object passed to the \\.data argument \\(the first argument\\) to be a data frame\\. Currently, the object being passed to .data has the class: integer. Please use the form mtcars %>% freq_table\\(am\\) or freq_table\\(mtcars, am\\)."
  )
})

testthat::test_that("Error when more than 2 column names are passed to the ... argument.", {
  testthat::expect_error(
    freq_table(mtcars, am, cyl, disp),
    "Currently, freq_table accepts one or two variables -- not more. You entered 3 into the ... argument."
  )
})

testthat::test_that("Error when no column names are passed to the ... argument.", {
  testthat::expect_error(
    freq_table(mtcars),
    "Did you pass any column names to the \\.\\.\\. argument\\? For example mtcars %>% freq_table\\(am\\) or freq_table\\(mtcars, am\\)"
  )
})


# =============================================================================
# Test that group_by no longer has an effect on results
# =============================================================================
df <- mtcars %>%
  group_by(am) %>%
  freq_table(cyl)

testthat::test_that("freq_table is ungrouping grouped input df's", {
  var <- unique(df$var)
  testthat::expect_match(var, "cyl")
})


# =============================================================================
# Test for explicit n = 0 for unobserved factor levels
# =============================================================================
df <- data.frame(
  cat_var = factor(
    c(rep("Always", 2), rep("Sometimes", 3)),
    levels = c("Always", "Sometimes", "Never")
  )
)

df <- df %>%
  freq_table(cat_var)

testthat::test_that("Unobserved factor levels are in the results table", {
  testthat::expect_true("Never" %in% df$cat)
  testthat::expect_equal(df$n, c(2, 3, 0))
})


# =============================================================================
# Test one-way frequency tables
# =============================================================================
df <- mtcars %>%
  freq_table(am)

testthat::test_that("Dimensions of the object returned by freq_table are as expected", {
  rows    <- nrow(df)
  columns <- ncol(df)

  testthat::expect_equal(rows, 2L)
  testthat::expect_equal(columns, 9L)
})

testthat::test_that("Class of freq_table_one_way is freq_table_one_way", {
  testthat::expect_is(df, "freq_table_one_way")
})

testthat::test_that("The correct var name is returned by freq_table", {
  name <- names(df)[1]
  testthat::expect_match(name, "var")
})

testthat::test_that("The correct cat var name is returned by freq_table", {
  name <- names(df)[2]
  testthat::expect_match(name, "cat")
})

testthat::test_that("The correct variables levels are returned by freq_table", {
  levels <- pull(df, cat)
  testthat::expect_equal(levels, c("0", "1"))
})

testthat::test_that("The correct default statistics are returned by freq_table", {
  n        <- pull(df, n)
  n_total  <- pull(df, n_total)
  percent  <- pull(df, percent) %>% round(2)
  se       <- pull(df, se) %>% round(2)
  t_crit   <- pull(df, t_crit) %>% round(2)
  lcl      <- pull(df, lcl) %>% round(2)
  ucl      <- pull(df, ucl) %>% round(2)

  testthat::expect_equal(n,       c(19, 13))
  testthat::expect_equal(n_total, c(32, 32))
  testthat::expect_equal(percent, c(59.38, 40.62))
  testthat::expect_equal(se,      c(8.82, 8.82))
  testthat::expect_equal(t_crit,  c(2.04, 2.04))
  testthat::expect_equal(lcl,     c(40.94, 24.50))
  testthat::expect_equal(ucl,     c(75.50, 59.06))
})

# Testing Wald CI's
# -----------------
df <- mtcars %>%
  freq_table(am, ci_type = "wald")

testthat::test_that("The correct Wald CI's are returned by freq_table", {
  lcl <- pull(df, lcl) %>% round(2)
  ucl <- pull(df, ucl) %>% round(2)

  testthat::expect_equal(lcl, c(41.38, 22.63))
  testthat::expect_equal(ucl, c(77.37, 58.62))
})




# =============================================================================
# Test two-way freq tables
# =============================================================================
df <- mtcars %>%
  freq_table(am, cyl)

testthat::test_that("Dimensions of the object returned by freq_table are as expected", {
  rows    <- nrow(df)
  columns <- ncol(df)

  testthat::expect_equal(rows, 6L)
  testthat::expect_equal(columns, 17L)
})

testthat::test_that("Class of freq_table_two_way is freq_table_two_way", {
  testthat::expect_is(df, "freq_table_two_way")
})

testthat::test_that("The correct var names are returned by freq_table", {
  row_var <- pull(df, row_var)
  col_var <- pull(df, col_var)

  testthat::expect_match(row_var, "am")
  testthat::expect_match(col_var, "cyl")
})

testthat::test_that("The correct variables levels are returned by freq_table", {
  row_cat <- pull(df, row_cat)
  col_cat <- pull(df, col_cat)

  testthat::expect_equal(row_cat, c("0", "0", "0", "1", "1", "1"))
  testthat::expect_equal(col_cat, c("4", "6", "8", "4", "6", "8"))
})

testthat::test_that("The correct default statistics are returned by freq_table", {
  n           <- pull(df, n)
  n_row       <- pull(df, n_row)
  n_total     <- pull(df, n_total)
  percent_row <- pull(df, percent_row) %>% round(2)
  lcl_row     <- pull(df, lcl_row) %>% round(2)
  ucl_row     <- pull(df, ucl_row) %>% round(2)

  testthat::expect_equal(n,           c(3, 4, 12, 8, 3, 2))
  testthat::expect_equal(n_row,       c(rep(19, 3), rep(13, 3)))
  testthat::expect_equal(n_total,     c(rep(32, 6)))
  testthat::expect_equal(percent_row, c(15.79, 21.05, 63.16, 61.54, 23.08, 15.38))
  testthat::expect_equal(lcl_row,     c(4.78, 7.58, 38.76, 32.30, 6.91, 3.43))
  testthat::expect_equal(ucl_row,     c(41.20, 46.44, 82.28, 84.29, 54.82, 48.18))
})

# Checking overall percents and CI's
# ----------------------------------
df <- mtcars %>%
  freq_table(am, cyl)

testthat::test_that("The correct overall percents and 95% CI's are returned", {
  percent_total <- pull(df, percent_total) %>% round(2)
  lcl_total     <- pull(df, lcl_total) %>% round(2)
  ucl_total     <- pull(df, ucl_total) %>% round(2)

  testthat::expect_equal(percent_total, c(9.38, 12.50, 37.50, 25.00, 9.38, 6.25))
  testthat::expect_equal(lcl_total,     c(2.86, 4.51, 21.97, 12.51, 2.86, 1.45))
  testthat::expect_equal(ucl_total,     c(26.66, 30.19, 56.11, 43.72, 26.66, 23.24))
})




# =============================================================================
# Check changing default parameters
# =============================================================================

# 99% confidence intervals instead of 95% confidence intervals
# ------------------------------------------------------------
df <- mtcars %>%
  freq_table(am, percent_ci = 99)

testthat::test_that("The 99% confidence intervals are correct", {
  lcl <- pull(df, lcl) %>% round(2)
  ucl <- pull(df, ucl) %>% round(2)

  testthat::expect_equal(lcl, c(34.89, 20.05))
  testthat::expect_equal(ucl, c(79.95, 65.11))
})


# =============================================================================
# Check optionally dropping factor levels
# =============================================================================

# Example: I want to retain factor levels with zero observations from the
# freq_table results
df <- data.frame(
  id = c(1, 2, 3, 4),
  gender = factor(c(1, 1, 1, 1), levels = c(1, 2), labels = c("female", "male"))
)

df <- df %>%
  freq_table(gender)

testthat::test_that("Unobserved factor level is returned by freq_table by default", {
  cats <- df$cat
  testthat::expect_equal(cats, c("female", "male"))
})

# Example: I want to drop factor levels with zero observations from the
# freq_table results
set.seed(123)
df <- data.frame(
  id = factor(rep(1:3, each = 4)),
  period = factor(rep(1:4)),
  x = factor(sample(c(0, 1), size = 12, replace = TRUE))
)

# Now, supppose we want to drop period 3 & 4 from my analysis.
# By default, this will give us 0's for period 3 & 4, but we want to drop them.
df <- df %>%
  filter(period %in% c(1, 2))

ft <- df %>%
  freq_table(period)

testthat::test_that("Unobserved factor level is returned by freq_table by default", {
  cats <- ft$cat
  testthat::expect_equal(cats, c("1", "2", "3", "4"))
})

ft <- df %>%
  freq_table(period, drop = TRUE)

testthat::test_that("Unobserved factor level is returned by freq_table when drop = FALSE", {
  cats <- ft$cat
  testthat::expect_equal(cats, c("1", "2"))
})


# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df, alpha, t)
detach("package:dplyr", unload=TRUE)
detach("package:freqtables", unload=TRUE)
