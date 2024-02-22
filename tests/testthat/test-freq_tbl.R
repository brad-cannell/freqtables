data("freq_study")

# =============================================================================
# Test error checks
#
# testthat evaluates the error string with a regular expression remember to
# escape special characters like ( with a double back slash.
# =============================================================================

# Not a data frame
# Return informative error
testthat::test_that("Error when a data frame isn't passed to the .data argument.", {
  testthat::expect_error(
    freq_study$sex |> freq_tbl(),
    "freq_tbl expects the object passed to the .data argument \\(the first argument) to be a data frame. Currently, the object being passed to .data has the class: factor. Please use the form mtcars |> freq_table(am) or freq_table(mtcars, am)."
  )
})

# Nothing is passed to .freq_var
# Return informative error
testthat::test_that("Error when nothing is passed to the .freq_var argument.", {
  testthat::expect_error(
    freq_study |> freq_tbl(),
    'argument "x" is missing, with no default'
  )
})

# A column name that doesn't exist in .data is passed to .freq_var
# Return informative error
testthat::test_that("Error when a column name that doesn't exist in .data is passed to .freq_var.", {
  testthat::expect_error(
    freq_study |> freq_tbl(age),
    "freq_tbl expects the object passed to the .freq_var argument \\(the second argument) to be a column of the data frame passed to .data. The object being passed to .freq_var is called 'age' and is not a column in the data frame being passed to .data."
  )
})

# Passing more than one column to freq_tbl
testthat::test_that("Error when passing more than one column to freq_tbl.", {
  testthat::expect_error(
    freq_study |> freq_tbl(exposure, sex),
    "object 'sex' not found"
  )
})


# =============================================================================
# Test one-way frequency tables
# =============================================================================

# One-way table - basic
# -----------------------------------------------------------------------------
one_way <- freq_study |> freq_tbl(sex)

testthat::test_that("Dimensions of one-way freq_tbl are as expected", {
  rows    <- nrow(one_way)
  columns <- ncol(one_way)

  testthat::expect_equal(rows, 3L)
  testthat::expect_equal(columns, 3L)
})


# One-way filter out missing
# -----------------------------------------------------------------------------
one_way_no_na <- freq_study |> dplyr::filter(!is.na(sex)) |> freq_tbl(sex)
