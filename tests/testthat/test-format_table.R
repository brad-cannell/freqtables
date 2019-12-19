library(dplyr)
library(freqtables)

data(mtcars)

context("test-format_table.R")

# =============================================================================
# Test one-way frequency table, stats = percent and ci (default)
# =============================================================================
df <- mtcars %>%
  freq_table(am) %>%
  format_table()

test_that("Dimensions of the object returned by format_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(2L, 3L))
})

test_that("The correct var name is returned by format_table", {
  var <- pull(df, var) %>% unique()
  expect_match(var, "am")
})

test_that("The correct variables levels are returned by format_table", {
  cat <- pull(df, cat)
  expect_equal(cat, c(0L, 1L))
})

test_that("The correct statistics are returned by format_table", {
  percent_95 <- pull(df, percent_95)
  expect_equal(percent_95, c("59.38 (40.94 - 75.50)", "40.62 (24.50 - 59.06)"))
})




# =============================================================================
# Test one-way frequency table, stats = n and percent
# =============================================================================
df <- mtcars %>%
  freq_table(am) %>%
  format_table(stats = "n and percent")

test_that("Dimensions of the object returned by format_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(2L, 3L))
})

test_that("The correct var name is returned by format_table", {
  var <- pull(df, var) %>% unique()
  expect_match(var, "am")
})

test_that("The correct variables levels are returned by format_table", {
  cat <- pull(df, cat)
  expect_equal(cat, c(0L, 1L))
})

test_that("The correct statistics are returned by format_table", {
  n_percent <- pull(df, n_percent)
  expect_equal(n_percent, c("19 (59.38)", "13 (40.62)"))
})




# =============================================================================
# Test two-way frequency table, stats = percent and ci (default)
# =============================================================================
df <- mtcars %>%
  freq_table(am, vs) %>%
  format_table()

test_that("Dimensions of the object returned by format_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(4L, 5L))
})

test_that("The correct var name is returned by format_table", {
  row_var <- pull(df, row_var) %>% unique()
  col_var <- pull(df, col_var) %>% unique()

  expect_match(row_var, "am")
  expect_match(col_var, "vs")
})

test_that("The correct variables levels are returned by format_table", {
  row_cat <- pull(df, row_cat)
  col_cat <- pull(df, col_cat)

  expect_equal(row_cat, c(0L, 0L, 1L, 1L))
  expect_equal(col_cat, c(0L, 1L, 0L, 1L))
})

test_that("The correct statistics are returned by format_table", {
  percent_row_95 <- pull(df, percent_row_95)
  expect_equal(percent_row_95, c("63.16 (38.76 - 82.28)", "36.84 (17.72 - 61.24)",
                                 "46.15 (20.83 - 73.63)", "53.85 (26.37 - 79.17)"))
})




# =============================================================================
# Test two-way frequency table, stats = n and percent
# =============================================================================
df <- mtcars %>%
  freq_table(am, vs, output = all) %>%
  format_table(stats = "n and percent")

test_that("Dimensions of the object returned by format_table are as expected", {
  dims <- dim(df)
  expect_equal(dims, c(4L, 5L))
})

test_that("The correct var name is returned by format_table", {
  row_var <- pull(df, row_var) %>% unique()
  col_var <- pull(df, col_var) %>% unique()

  expect_match(row_var, "am")
  expect_match(col_var, "vs")
})

test_that("The correct variables levels are returned by format_table", {
  row_cat <- pull(df, row_cat)
  col_cat <- pull(df, col_cat)

  expect_equal(row_cat, c(0L, 0L, 1L, 1L))
  expect_equal(col_cat, c(0L, 1L, 0L, 1L))
})

test_that("The correct statistics are returned by format_table", {
  n_percent_total <- pull(df, n_percent_total)
  expect_equal(n_percent_total, c("12 (37.50)", " 7 (21.88)",
                                  " 6 (18.75)", " 7 (21.88)"))
})


# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df)
detach("package:dplyr", unload=TRUE)
detach("package:freqtables", unload=TRUE)
