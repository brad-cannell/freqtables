library(dplyr)
library(freqtables)

testthat::context("test-freq_table.R")

testthat::test_that("freq_tbl returns one-way counts and proportions", {
  df <- mtcars %>% freq_tbl(am)

  testthat::expect_equal(names(df), c("am", "n", "prop"))
  testthat::expect_equal(df$n, c(19L, 13L))
  testthat::expect_equal(round(df$prop, 4), c(0.5938, 0.4062))
})

testthat::test_that("freq_tbl works with group_by", {
  df <- mtcars %>%
    group_by(cyl) %>%
    freq_tbl(am)

  testthat::expect_true(all(c("cyl", "am", "n", "n_group", "prop_group") %in% names(df)))
  testthat::expect_equal(unique(df$n_group), c(11L, 7L, 14L))
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
