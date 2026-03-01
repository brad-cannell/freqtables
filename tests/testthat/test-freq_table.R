library(dplyr)
library(freqtables)

data(mtcars)

testthat::context("test-freq_table.R")

# =============================================================================
# Test error checks
# =============================================================================
testthat::test_that("Error when a data frame isn't passed to the .data argument.", {
  testthat::expect_error(
    freq_table(1:10, am),
    "freq_table expects a data frame"
  )
})

testthat::test_that("Error when more than 1 column name is passed (deprecated).", {
  testthat::expect_error(
    freq_table(mtcars, am, cyl),
    "Passing more than one column name to the freq_table function is deprecated"
  )
})

testthat::test_that("Error when no column names are passed to the ... argument.", {
  testthat::expect_error(
    freq_table(mtcars),
    "No column names were passed to freq_table"
  )
})


# =============================================================================
# Test one-way frequency table (ungrouped, default options)
# =============================================================================
df <- mtcars %>%
  freq_table(am)

testthat::test_that("One-way freq_table has correct dimensions", {
  testthat::expect_equal(nrow(df), 2L)
  testthat::expect_equal(ncol(df), 5L) # am, n, prop, lcl, ucl
})

testthat::test_that("One-way freq_table has correct class", {
  testthat::expect_is(df, "freq_table_one_way")
})

testthat::test_that("One-way freq_table uses variable name as column header", {
  testthat::expect_true("am" %in% names(df))
})

testthat::test_that("One-way freq_table returns proportions by default (not percentages)", {
  prop <- pull(df, prop) %>% round(4)
  testthat::expect_equal(prop, c(0.5938, 0.4062))
})

testthat::test_that("One-way freq_table returns logit CI by default", {
  lcl <- pull(df, lcl) %>% round(4)
  ucl <- pull(df, ucl) %>% round(4)
  testthat::expect_equal(lcl, c(0.4094, 0.2450))
  testthat::expect_equal(ucl, c(0.7550, 0.5906))
})


# =============================================================================
# Test one-way frequency table with Wald CI
# =============================================================================
df_wald <- mtcars %>%
  freq_table(am, ci_type = "wald")

testthat::test_that("Wald CI returns correct values for one-way table", {
  lcl <- pull(df_wald, lcl) %>% round(4)
  ucl <- pull(df_wald, ucl) %>% round(4)
  testthat::expect_equal(lcl, c(0.4138, 0.2263))
  testthat::expect_equal(ucl, c(0.7737, 0.5862))
})


# =============================================================================
# Test one-way frequency table with percent = TRUE
# =============================================================================
df_pct <- mtcars %>%
  freq_table(am, percent = TRUE)

testthat::test_that("percent = TRUE returns percentages", {
  testthat::expect_true("percent" %in% names(df_pct))
  pct <- pull(df_pct, percent) %>% round(2)
  testthat::expect_equal(pct, c(59.38, 40.62))
})


# =============================================================================
# Test one-way frequency table with se = TRUE and critical_value = TRUE
# =============================================================================
df_verbose <- mtcars %>%
  freq_table(am, se = TRUE, critical_value = TRUE)

testthat::test_that("se and critical_value parameters add columns", {
  testthat::expect_true("se" %in% names(df_verbose))
  testthat::expect_true("t_crit" %in% names(df_verbose))
})


# =============================================================================
# Test one-way frequency table with generic_col_names = TRUE
# =============================================================================
df_generic <- mtcars %>%
  freq_table(am, generic_col_names = TRUE)

testthat::test_that("generic_col_names = TRUE returns generic column names", {
  testthat::expect_true("col" %in% names(df_generic))
  testthat::expect_true("cat" %in% names(df_generic))
  testthat::expect_equal(unique(df_generic$col), "am")
})


# =============================================================================
# Test grouped frequency table (using group_by)
# =============================================================================
df_grouped <- mtcars %>%
  group_by(cyl) %>%
  freq_table(am)

testthat::test_that("Grouped freq_table has correct dimensions", {
  testthat::expect_equal(nrow(df_grouped), 6L)
  testthat::expect_equal(ncol(df_grouped), 7L) # cyl, am, n, n_group, prop_group, lcl_group, ucl_group
})

testthat::test_that("Grouped freq_table has correct class", {
  testthat::expect_is(df_grouped, "freq_table_two_way")
})

testthat::test_that("Grouped freq_table uses variable names as column headers", {
  testthat::expect_true("cyl" %in% names(df_grouped))
  testthat::expect_true("am" %in% names(df_grouped))
})

testthat::test_that("Grouped freq_table returns correct n values", {
  n <- pull(df_grouped, n)
  testthat::expect_equal(n, c(3, 8, 4, 3, 12, 2))
})

testthat::test_that("Grouped freq_table returns correct n_group values", {
  n_group <- pull(df_grouped, n_group)
  testthat::expect_equal(n_group, c(11, 11, 7, 7, 14, 14))
})

testthat::test_that("Grouped freq_table returns correct prop_group values", {
  prop_group <- pull(df_grouped, prop_group) %>% round(3)
  testthat::expect_equal(prop_group, c(0.273, 0.727, 0.571, 0.429, 0.857, 0.143))
})

testthat::test_that("Grouped freq_table returns correct CI values", {
  lcl <- pull(df_grouped, lcl_group) %>% round(3)
  ucl <- pull(df_grouped, ucl_group) %>% round(3)
  testthat::expect_equal(lcl, c(0.081, 0.385, 0.199, 0.122, 0.544, 0.032))
  testthat::expect_equal(ucl, c(0.615, 0.919, 0.878, 0.801, 0.968, 0.456))
})


# =============================================================================
# Test grouped freq_table with generic_col_names = TRUE
# =============================================================================
df_gen_grouped <- mtcars %>%
  group_by(cyl) %>%
  freq_table(am, generic_col_names = TRUE)

testthat::test_that("Grouped generic_col_names returns correct column names", {
  testthat::expect_true("group_01_col" %in% names(df_gen_grouped))
  testthat::expect_true("group_01_cat" %in% names(df_gen_grouped))
  testthat::expect_true("col" %in% names(df_gen_grouped))
  testthat::expect_true("cat" %in% names(df_gen_grouped))
  testthat::expect_equal(unique(df_gen_grouped$group_01_col), "cyl")
  testthat::expect_equal(unique(df_gen_grouped$col), "am")
})


# =============================================================================
# Test grouped freq_table with overall = TRUE
# =============================================================================
df_overall <- mtcars %>%
  group_by(cyl) %>%
  freq_table(am, overall = TRUE)

testthat::test_that("overall = TRUE adds overall columns", {
  testthat::expect_true("n_total" %in% names(df_overall))
  testthat::expect_true("prop_overall" %in% names(df_overall))
  testthat::expect_true("lcl_overall" %in% names(df_overall))
  testthat::expect_true("ucl_overall" %in% names(df_overall))
})


# =============================================================================
# Test multiple grouping variables
# =============================================================================
df_multi <- mtcars %>%
  group_by(cyl, vs) %>%
  freq_table(am)

testthat::test_that("Multiple grouping variables work correctly", {
  testthat::expect_true("cyl" %in% names(df_multi))
  testthat::expect_true("vs" %in% names(df_multi))
  testthat::expect_true("am" %in% names(df_multi))
  testthat::expect_is(df_multi, "freq_table_two_way")
})

df_multi_generic <- mtcars %>%
  group_by(cyl, vs) %>%
  freq_table(am, generic_col_names = TRUE)

testthat::test_that("Multiple grouping variables with generic names", {
  testthat::expect_true("group_01_col" %in% names(df_multi_generic))
  testthat::expect_true("group_01_cat" %in% names(df_multi_generic))
  testthat::expect_true("group_02_col" %in% names(df_multi_generic))
  testthat::expect_true("group_02_cat" %in% names(df_multi_generic))
  testthat::expect_equal(unique(df_multi_generic$group_01_col), "cyl")
  testthat::expect_equal(unique(df_multi_generic$group_02_col), "vs")
})


# =============================================================================
# Test factor level handling (drop parameter)
# =============================================================================
df_factor <- data.frame(
  id = c(1, 2, 3, 4),
  gender = factor(c(1, 1, 1, 1), levels = c(1, 2), labels = c("female", "male"))
)

df_result <- df_factor %>%
  freq_table(gender)

testthat::test_that("Unobserved factor level is returned by default", {
  testthat::expect_equal(nrow(df_result), 2L)
})

df_result_drop <- df_factor %>%
  freq_table(gender, drop = TRUE)

testthat::test_that("Unobserved factor level is dropped when drop = TRUE", {
  testthat::expect_equal(nrow(df_result_drop), 1L)
})


# =============================================================================
# Test 99% confidence interval
# =============================================================================
df_99 <- mtcars %>%
  freq_table(am, percent_ci = 99)

testthat::test_that("99% CI is wider than 95% CI", {
  testthat::expect_true(df_99$lcl[1] < df$lcl[1])
  testthat::expect_true(df_99$ucl[1] > df$ucl[1])
})


# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df, df_wald, df_pct, df_verbose, df_generic, df_grouped,
   df_gen_grouped, df_overall, df_multi, df_multi_generic,
   df_factor, df_result, df_result_drop, df_99)
detach("package:dplyr", unload = TRUE)
detach("package:freqtables", unload = TRUE)
