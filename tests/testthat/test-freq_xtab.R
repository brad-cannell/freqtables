library(dplyr)
library(freqtables)

data(mtcars)

testthat::context("test-freq_xtab.R")

# =============================================================================
# Test error checks
# =============================================================================
testthat::test_that("Error when a data frame isn't passed", {
  testthat::expect_error(
    freq_xtab(1:10, am, cyl),
    "freq_xtab expects a data frame"
  )
})


# =============================================================================
# Test basic contingency table
# =============================================================================
ct <- mtcars %>%
  freq_xtab(am, cyl)

testthat::test_that("freq_xtab returns a table/matrix", {
  testthat::expect_true(is.table(ct) || is.matrix(ct))
})

testthat::test_that("freq_xtab includes marginal totals by default", {
  # Should have a "Sum" row and column
  testthat::expect_true("Sum" %in% rownames(ct))
  testthat::expect_true("Sum" %in% colnames(ct))
})

testthat::test_that("freq_xtab has correct dimension names", {
  testthat::expect_equal(names(dimnames(ct))[1], "am")
  testthat::expect_equal(names(dimnames(ct))[2], "cyl")
})

testthat::test_that("freq_xtab cell counts sum to correct total", {
  # Total should be 32 (nrow(mtcars))
  testthat::expect_equal(ct["Sum", "Sum"], 32L)
})


# =============================================================================
# Test without margins
# =============================================================================
ct_no_margins <- mtcars %>%
  freq_xtab(am, cyl, margins = FALSE)

testthat::test_that("freq_xtab without margins has no Sum row/col", {
  testthat::expect_false("Sum" %in% rownames(ct_no_margins))
  testthat::expect_false("Sum" %in% colnames(ct_no_margins))
})


# =============================================================================
# Test grouped contingency table
# =============================================================================
ct_grouped <- mtcars %>%
  group_by(vs) %>%
  freq_xtab(am, cyl)

testthat::test_that("Grouped freq_xtab returns a list", {
  testthat::expect_true(is.list(ct_grouped))
})

testthat::test_that("Grouped freq_xtab returns one table per group level", {
  testthat::expect_equal(length(ct_grouped), 2L) # vs has 2 levels: 0, 1
})

testthat::test_that("Each grouped table is a table/matrix", {
  testthat::expect_true(is.table(ct_grouped[[1]]) || is.matrix(ct_grouped[[1]]))
  testthat::expect_true(is.table(ct_grouped[[2]]) || is.matrix(ct_grouped[[2]]))
})


# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, ct, ct_no_margins, ct_grouped)
detach("package:dplyr", unload = TRUE)
detach("package:freqtables", unload = TRUE)
