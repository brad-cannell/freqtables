library(dplyr)
library(freqtables)

data(mtcars)

testthat::context("test-freq_tbl.R")

# =============================================================================
# Test error checks
# =============================================================================
testthat::test_that("Error when a data frame isn't passed to .data", {
  testthat::expect_error(
    freq_tbl(1:10, am),
    "freq_tbl expects a data frame"
  )
})

testthat::test_that("Error when multiple columns are passed", {
  testthat::expect_error(
    freq_tbl(mtcars, am, cyl),
    "freq_tbl\\(\\) accepts only one column name"
  )
})


# =============================================================================
# Test one-way frequency tables (ungrouped)
# =============================================================================
df <- mtcars %>%
  freq_tbl(am)

testthat::test_that("One-way freq_tbl has correct dimensions", {
  testthat::expect_equal(nrow(df), 2L)
  testthat::expect_equal(ncol(df), 3L) # am, n, prop
})

testthat::test_that("One-way freq_tbl has correct class", {
  testthat::expect_is(df, "freq_tbl")
})

testthat::test_that("One-way freq_tbl has variable name as column header", {
  testthat::expect_true("am" %in% names(df))
})

testthat::test_that("One-way freq_tbl returns correct columns", {
  testthat::expect_equal(names(df), c("am", "n", "prop"))
})

testthat::test_that("One-way freq_tbl returns correct statistics", {
  n    <- pull(df, n)
  prop <- pull(df, prop) %>% round(4)

  testthat::expect_equal(n,    c(19, 13))
  testthat::expect_equal(prop, c(0.5938, 0.4062))
})


# =============================================================================
# Test two-way frequency tables (grouped with group_by)
# =============================================================================
df <- mtcars %>%
  group_by(cyl) %>%
  freq_tbl(am)

testthat::test_that("Grouped freq_tbl has correct dimensions", {
  testthat::expect_equal(nrow(df), 6L)
  testthat::expect_equal(ncol(df), 5L) # cyl, am, n, n_group, prop_group
})

testthat::test_that("Grouped freq_tbl has correct column names", {
  testthat::expect_equal(names(df), c("cyl", "am", "n", "n_group", "prop_group"))
})

testthat::test_that("Grouped freq_tbl returns correct n values", {
  n <- pull(df, n)
  testthat::expect_equal(n, c(3, 8, 4, 3, 12, 2))
})

testthat::test_that("Grouped freq_tbl returns correct n_group values", {
  n_group <- pull(df, n_group)
  testthat::expect_equal(n_group, c(11, 11, 7, 7, 14, 14))
})

testthat::test_that("Grouped freq_tbl returns correct prop_group values", {
  prop_group <- pull(df, prop_group) %>% round(3)
  testthat::expect_equal(prop_group, c(0.273, 0.727, 0.571, 0.429, 0.857, 0.143))
})


# =============================================================================
# Test N-way frequency tables (multiple grouping variables)
# =============================================================================
df <- mtcars %>%
  group_by(cyl, vs) %>%
  freq_tbl(am)

testthat::test_that("N-way freq_tbl has correct column names", {
  testthat::expect_equal(names(df), c("cyl", "vs", "am", "n", "n_group", "prop_group"))
})

testthat::test_that("N-way freq_tbl has correct class", {
  testthat::expect_is(df, "freq_tbl")
})


# =============================================================================
# Test factor level handling
# =============================================================================
df_factor <- data.frame(
  cat_var = factor(
    c(rep("Always", 2), rep("Sometimes", 3)),
    levels = c("Always", "Sometimes", "Never")
  )
)

df_result <- df_factor %>%
  freq_tbl(cat_var)

testthat::test_that("Unobserved factor levels appear with n=0 by default", {
  testthat::expect_true("Never" %in% df_result$cat_var)
  testthat::expect_equal(df_result$n, c(2, 3, 0))
})

df_result_drop <- df_factor %>%
  freq_tbl(cat_var, drop = TRUE)

testthat::test_that("Unobserved factor levels are dropped when drop = TRUE", {
  testthat::expect_false("Never" %in% df_result_drop$cat_var)
  testthat::expect_equal(nrow(df_result_drop), 2L)
})


# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df, df_factor, df_result, df_result_drop)
detach("package:dplyr", unload = TRUE)
detach("package:freqtables", unload = TRUE)
