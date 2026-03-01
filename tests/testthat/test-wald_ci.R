library(dplyr)
library(freqtables)

data(mtcars)

testthat::context("test-wald_ci.R")

# =============================================================================
# Test error checks
# =============================================================================
testthat::test_that("Error when non-data frame is passed", {
  testthat::expect_error(wald_ci(1:10), "wald_ci expects a data frame")
})

testthat::test_that("Error when data frame lacks prop or prop_group column", {
  testthat::expect_error(
    wald_ci(data.frame(x = 1:3)),
    "Did you run freq_tbl\\(\\) first"
  )
})


# =============================================================================
# Test Wald CI for ungrouped data
# =============================================================================
df <- mtcars %>%
  freq_tbl(am) %>%
  wald_ci()

testthat::test_that("Wald CI adds correct columns for ungrouped data", {
  testthat::expect_true(all(c("se", "t_crit", "lcl", "ucl") %in% names(df)))
})

testthat::test_that("Wald CI returns correct values for ungrouped data", {
  # Expected: prop = 19/32 = 0.59375, se = sqrt(0.59375*0.40625/31)
  se_expected <- sqrt(0.59375 * 0.40625 / 31)
  t_crit_expected <- qt(0.975, df = 31)
  lcl_expected <- 0.59375 - t_crit_expected * se_expected
  ucl_expected <- 0.59375 + t_crit_expected * se_expected

  testthat::expect_equal(round(df$se[1], 4), round(se_expected, 4))
  testthat::expect_equal(round(df$lcl[1], 4), round(lcl_expected, 4))
  testthat::expect_equal(round(df$ucl[1], 4), round(ucl_expected, 4))
})


# =============================================================================
# Test Wald CI for grouped data
# =============================================================================
df_grouped <- mtcars %>%
  group_by(cyl) %>%
  freq_tbl(am) %>%
  wald_ci()

testthat::test_that("Wald CI adds correct columns for grouped data", {
  testthat::expect_true(all(c("se_group", "t_crit_group", "lcl_group", "ucl_group") %in% names(df_grouped)))
})

testthat::test_that("Wald CI for grouped data uses group n for degrees of freedom", {
  # For cyl=4: n_group=11, prop=3/11=0.2727
  prop_val <- 3 / 11
  se_val <- sqrt(prop_val * (1 - prop_val) / (11 - 1))
  t_val <- qt(0.975, df = 10)
  lcl_val <- prop_val - t_val * se_val
  ucl_val <- prop_val + t_val * se_val

  testthat::expect_equal(round(df_grouped$lcl_group[1], 3), round(lcl_val, 3))
  testthat::expect_equal(round(df_grouped$ucl_group[1], 3), round(ucl_val, 3))
})


# =============================================================================
# Test custom confidence level
# =============================================================================
df_99 <- mtcars %>%
  freq_tbl(am) %>%
  wald_ci(percent_ci = 99)

testthat::test_that("Wald CI respects custom percent_ci", {
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
