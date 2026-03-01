library(dplyr)
library(freqtables)

data(mtcars)

testthat::context("test-freq_test.R")

# =============================================================================
# Test one-way frequency tables
# =============================================================================
df <- mtcars %>%
  freq_table(am) %>%
  freq_test()

testthat::test_that("freq_test one-way has correct class", {
  testthat::expect_is(df, "freq_table_one_way")
})

testthat::test_that("freq_test one-way adds correct columns", {
  testthat::expect_true("n_expected" %in% names(df))
  testthat::expect_true("chi2_contrib" %in% names(df))
  testthat::expect_true("chi2_pearson" %in% names(df))
  testthat::expect_true("df" %in% names(df))
  testthat::expect_true("p_chi2_pearson" %in% names(df))
})

testthat::test_that("freq_test one-way returns correct statistics", {
  n_expected     <- pull(df, n_expected)
  chi2_contrib   <- pull(df, chi2_contrib)
  chi2_pearson   <- pull(df, chi2_pearson)
  deg_freedom    <- pull(df, df)
  p_chi2_pearson <- pull(df, p_chi2_pearson) %>% round(7)

  testthat::expect_equal(n_expected,     rep(16, 2))
  testthat::expect_equal(chi2_contrib,   rep(0.5625, 2))
  testthat::expect_equal(chi2_pearson,   rep(1.125, 2))
  testthat::expect_equal(deg_freedom,    rep(1, 2))
  testthat::expect_equal(p_chi2_pearson, rep(0.2888444, 2))
})


# =============================================================================
# Test two-way freq tables
# =============================================================================
df2 <- mtcars %>%
  group_by(am) %>%
  freq_table(cyl) %>%
  freq_test()

testthat::test_that("freq_test two-way has correct class", {
  testthat::expect_is(df2, "freq_table_two_way")
})

testthat::test_that("freq_test two-way adds chi-square columns", {
  testthat::expect_true("n_expected" %in% names(df2))
  testthat::expect_true("chi2_pearson" %in% names(df2))
  testthat::expect_true("p_chi2_pearson" %in% names(df2))
})

testthat::test_that("freq_test two-way returns correct chi-square statistic", {
  chi2_pearson   <- pull(df2, chi2_pearson) %>% unique() %>% round(6)
  testthat::expect_equal(chi2_pearson, 8.740733)
})

testthat::test_that("freq_test two-way returns correct degrees of freedom", {
  deg_freedom <- pull(df2, df) %>% unique()
  testthat::expect_equal(deg_freedom, 2)
})

testthat::test_that("freq_test two-way returns correct p-value", {
  p_val <- pull(df2, p_chi2_pearson) %>% unique() %>% round(8)
  testthat::expect_equal(p_val, 0.01264661)
})

testthat::test_that("freq_test two-way includes Fisher's test when expected counts <= 5", {
  # With am (2 levels) x cyl (3 levels), some expected counts may be <= 5
  testthat::expect_true("p_fisher" %in% names(df2))
})


# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df, df2)
detach("package:dplyr", unload = TRUE)
detach("package:freqtables", unload = TRUE)
