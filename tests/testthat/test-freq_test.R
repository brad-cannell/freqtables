library(dplyr)
library(freqtables)

data(mtcars)

context("test-freq_test.R")

# =============================================================================
# Test one-way frequency tables
# =============================================================================
df <- mtcars %>%
  group_by(am) %>%
  freq_table() %>%
  freq_test()

test_that("Dimensions of the object returned by freq_test are as expected", {
  rows    <- nrow(df)
  columns <- ncol(df)

  expect_equal(rows, 2L)
  expect_equal(columns, 12L)
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

test_that("The correct variables levels are returned by freq_test", {
  levels <- pull(df, cat)
  expect_equal(levels, c(0, 1))
})

test_that("The correct default statistics are returned by freq_test", {
  n_expected     <- pull(df, n_expected)
  chi2_contrib   <- pull(df, chi2_contrib)
  chi2_pearson   <- pull(df, chi2_pearson)
  deg_freedom    <- pull(df, df)
  p_chi2_pearson <- pull(df, p_chi2_pearson) %>% round(7)

  expect_equal(n_expected,     rep(16, 2))
  expect_equal(chi2_contrib,   rep(0.5625, 2))
  expect_equal(chi2_pearson,   rep(1.125, 2))
  expect_equal(deg_freedom,    rep(1, 2))
  expect_equal(p_chi2_pearson, rep(0.2888444, 2))
})




# =============================================================================
# Test two-way freq tables
# =============================================================================
df <- mtcars %>%
  group_by(am, cyl) %>%
  freq_table()%>%
  freq_test()

test_that("Dimensions of the object returned by freq_table are as expected", {
  rows    <- nrow(df)
  columns <- ncol(df)

  expect_equal(rows, 6L)
  expect_equal(columns, 18L)
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
  n_col          <- pull(df, n_col)
  n_expected     <- pull(df, n_expected)
  chi2_contrib   <- pull(df, chi2_contrib)
  chi2_pearson   <- pull(df, chi2_pearson)
  r_column       <- pull(df, r)
  c_column       <- pull(df, c)
  deg_freedom    <- pull(df, df)
  p_chi2_pearson <- pull(df, p_chi2_pearson) %>% round(7)

  expect_equal(n_col,          c(11, 7, 14, 11, 7, 14))
  expect_equal(n_expected,     c(6.53125, 4.15625, 8.31250, 4.46875, 2.84375, 5.68750))
  expect_equal(chi2_contrib,   c(1.909240431, 0.005874060, 1.635808271, 2.790428322,
                                 0.008585165, 2.390796703))
  expect_equal(chi2_pearson,   rep(8.740733, 6))
  expect_equal(r_column,       rep(2, 6))
  expect_equal(c_column,       rep(3, 6))
  expect_equal(deg_freedom,    rep(2, 6))
  expect_equal(p_chi2_pearson, rep(0.01264661, 6))
})

# Checking Fisher's Exact Method
# ------------------------------
df <- mtcars %>%
  group_by(am, cyl) %>%
  freq_table()%>%
  freq_test(method = "fisher")

test_that("The expected p-value is returned from the fisher method", {
  fisher_p_value <- pull(df, p_fisher)
  expect_equal(fisher_p_value, rep(0.009104702, 6))
})




# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df, alpha, t)
detach("package:dplyr", unload=TRUE)
detach("package:freqtables", unload=TRUE)
