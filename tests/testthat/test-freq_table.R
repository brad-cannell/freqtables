library(dplyr)
library(freqtables)

testthat::context("test-freq_table.R")

testthat::test_that("freq_tbl returns one-way counts and proportions", {
  df <- mtcars %>% freq_tbl(am)

  testthat::expect_equal(names(df), c("am", "n", "prop"))
  testthat::expect_false("percent" %in% names(df))
  testthat::expect_equal(df$n, c(19L, 13L))
  testthat::expect_equal(round(df$prop, 4), c(0.5938, 0.4062))
})

testthat::test_that("freq_tbl percent=TRUE renames prop to percent", {
  df <- mtcars %>% freq_tbl(am, percent = TRUE)

  testthat::expect_equal(names(df), c("am", "n", "percent"))
  testthat::expect_false("prop" %in% names(df))
  testthat::expect_equal(round(df$percent, 2), c(59.38, 40.62))
})

testthat::test_that("freq_tbl works with group_by", {
  df <- mtcars %>%
    group_by(cyl) %>%
    freq_tbl(am)

  testthat::expect_true(all(c("cyl", "am", "n", "n_group", "prop_group") %in% names(df)))
  testthat::expect_equal(unique(df$n_group), c(11L, 7L, 14L))
})

testthat::test_that("grouped freq_tbl percent=TRUE renames prop_group", {
  df <- mtcars %>%
    group_by(cyl) %>%
    freq_tbl(am, percent = TRUE)

  testthat::expect_true(all(c("cyl", "am", "n", "n_group", "percent_group") %in% names(df)))
  testthat::expect_false("prop_group" %in% names(df))
  testthat::expect_equal(round(df$percent_group, 2), c(27.27, 72.73, 57.14, 42.86, 85.71, 14.29))
})

testthat::test_that("grouped overall freq_tbl percent=TRUE renames all prop columns", {
  df <- mtcars %>%
    group_by(cyl) %>%
    freq_tbl(am, overall = TRUE, percent = TRUE)

  testthat::expect_true(all(c("percent_group", "percent_total") %in% names(df)))
  testthat::expect_false(any(c("prop_group", "prop_total") %in% names(df)))
  testthat::expect_equal(round(df$percent_total, 2), c(9.38, 25.00, 12.50, 9.38, 37.50, 6.25))
})

testthat::test_that("freq_tbl percent renaming works with generic column names", {
  df <- mtcars %>%
    group_by(cyl) %>%
    freq_tbl(am, percent = TRUE, generic_col_names = TRUE)

  testthat::expect_true("percent_group" %in% names(df))
  testthat::expect_false("prop_group" %in% names(df))
})

testthat::test_that("freq_table rejects more than one analysis variable", {
  testthat::expect_error(
    freq_table(mtcars, cyl, am),
    "Passing more than one column name to freq_table\\(\\) is deprecated"
  )
})

testthat::test_that("freq_table defaults to grouped CI output", {
  df <- mtcars %>%
    group_by(cyl) %>%
    freq_table(am)

  testthat::expect_s3_class(df, "freq_table_two_way")
  testthat::expect_true(all(c("cyl", "am", "n", "n_group", "prop_group", "lcl_group", "ucl_group") %in% names(df)))
})

testthat::test_that("grouped freq_table percent=TRUE renames prop_group", {
  df <- mtcars %>%
    group_by(cyl) %>%
    freq_table(am, percent = TRUE)

  testthat::expect_true(all(c("cyl", "am", "n", "n_group", "percent_group", "lcl_group", "ucl_group") %in% names(df)))
  testthat::expect_false("prop_group" %in% names(df))
  testthat::expect_equal(round(df$percent_group, 2), c(27.27, 72.73, 57.14, 42.86, 85.71, 14.29))
})

testthat::test_that("grouped overall freq_table percent=TRUE renames all prop columns", {
  df <- mtcars %>%
    group_by(cyl) %>%
    freq_table(am, overall = TRUE, percent = TRUE)

  testthat::expect_true(all(c("percent_group", "percent_total", "lcl_group", "ucl_group") %in% names(df)))
  testthat::expect_false(any(c("lcl_total", "ucl_total") %in% names(df)))
  testthat::expect_false(any(c("prop_group", "prop_total") %in% names(df)))
  testthat::expect_equal(round(df$percent_total, 2), c(9.38, 25.00, 12.50, 9.38, 37.50, 6.25))
})

testthat::test_that("freq_table percent renaming works with generic column names", {
  df <- mtcars %>%
    group_by(cyl) %>%
    freq_table(am, percent = TRUE, generic_col_names = TRUE)

  testthat::expect_true("percent_group" %in% names(df))
  testthat::expect_false("prop_group" %in% names(df))
})

testthat::test_that("wald_ci and logit_ci both add CI columns", {
  base <- mtcars %>% freq_tbl(am, overall = TRUE)
  w <- wald_ci(base)
  l <- logit_ci(base)

  testthat::expect_true(all(c("lcl", "ucl") %in% names(w)))
  testthat::expect_true(all(c("lcl", "ucl") %in% names(l)))
})

testthat::test_that("freq_tables returns nested tables", {
  out <- freq_tables(mtcars, am, cyl)

  testthat::expect_equal(nrow(out), 2)
  testthat::expect_true(all(c("col", "freq_table") %in% names(out)))
  testthat::expect_true(all(vapply(out$freq_table, inherits, logical(1), "data.frame")))
})
