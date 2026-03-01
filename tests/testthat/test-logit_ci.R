library(dplyr)
library(freqtables)

data(mtcars)

testthat::context("test-logit_ci.R")

# =============================================================================
# Test error checks
# =============================================================================
testthat::test_that("Error when non-data frame is passed", {
  testthat::expect_error(logit_ci(1:10), "logit_ci expects a data frame")
})

testthat::test_that("Error when data frame lacks prop or prop_group column", {
  testthat::expect_error(
    logit_ci(data.frame(x = 1:3)),
    "Did you run freq_tbl\\(\\) first"
  )
})


# =============================================================================
# Test Logit CI for ungrouped data
# =============================================================================
df <- mtcars %>%
  freq_tbl(am) %>%
  logit_ci()

testthat::test_that("Logit CI adds correct columns for ungrouped data", {
  testthat::expect_true(all(c("se", "t_crit", "lcl", "ucl") %in% names(df)))
})

testthat::test_that("Logit CI returns correct values for ungrouped data", {
  # These should match the old freq_table logit CI values (on proportion scale)
  lcl <- pull(df, lcl) %>% round(4)
  ucl <- pull(df, ucl) %>% round(4)

  testthat::expect_equal(lcl, c(0.4094, 0.2450))
  testthat::expect_equal(ucl, c(0.7550, 0.5906))
})


# =============================================================================
# Test Logit CI for grouped data
# =============================================================================
df_grouped <- mtcars %>%
  group_by(cyl) %>%
  freq_tbl(am) %>%
  logit_ci()

testthat::test_that("Logit CI adds correct columns for grouped data", {
  testthat::expect_true(all(c("se_group", "t_crit_group", "lcl_group", "ucl_group") %in% names(df_grouped)))
})

testthat::test_that("Logit CI intermediate values are removed from output", {
  # prop_log, se_log, etc. should not appear in the output
  testthat::expect_false("prop_log" %in% names(df_grouped))
  testthat::expect_false("se_log" %in% names(df_grouped))
  testthat::expect_false("prop_log_group" %in% names(df_grouped))
  testthat::expect_false("se_log_group" %in% names(df_grouped))
})


# =============================================================================
# Test custom confidence level
# =============================================================================
df_99 <- mtcars %>%
  freq_tbl(am) %>%
  logit_ci(percent_ci = 99)

testthat::test_that("Logit CI respects custom percent_ci", {
  # 99% CI should be wider than 95%
  testthat::expect_true(df_99$lcl[1] < df$lcl[1])
  testthat::expect_true(df_99$ucl[1] > df$ucl[1])
})


# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df, df_grouped, df_99)
detach("package:dplyr", unload = TRUE)
detach("package:freqtables", unload = TRUE)
