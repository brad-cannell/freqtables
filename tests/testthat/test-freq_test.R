library(dplyr)
library(freqtables)

testthat::context("test-freq_test.R")

testthat::test_that("freq_test works for one-way freq_table output", {
  df <- mtcars %>%
    freq_table(am, overall = TRUE) %>%
    freq_test()

  testthat::expect_s3_class(df, "freq_table_one_way")
  testthat::expect_true(all(c("n_expected", "chi2_contrib", "chi2_pearson", "p_chi2_pearson") %in% names(df)))
})

testthat::test_that("freq_test works for grouped freq_table output", {
  df <- mtcars %>%
    group_by(am) %>%
    freq_table(cyl) %>%
    freq_test()

  testthat::expect_s3_class(df, "freq_table_two_way")
  testthat::expect_true(all(c("n_col", "n_expected", "chi2_pearson", "p_chi2_pearson") %in% names(df)))
})
