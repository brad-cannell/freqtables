library(dplyr)
library(freqtables)

data(mtcars)

testthat::context("test-freq_test.R")

# =============================================================================
# Get sample size for cars with 4 cylinders
# =============================================================================
testthat::test_that("Group n is returned as expected", {
  n <- mtcars %>% get_group_n(cyl == 4)
  testthat::expect_match(n, "N = 11")
})


# =============================================================================
# Clean up
# =============================================================================
rm(mtcars, df)
detach("package:dplyr", unload=TRUE)
detach("package:freqtables", unload=TRUE)
