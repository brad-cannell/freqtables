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
    "freq_tbl\\() expects the value passed to the '.data' argument \\(the first argument) to be a data frame. Currently, the value being passed to '.data' has the class: 'factor'. Please use the form 'mtcars |> freq_tbl(am)' or 'freq_tbl(mtcars, am)'."
  )
})

# Nothing is passed to .freq_var
# Return informative error
testthat::test_that("Error when nothing is passed to the .freq_var argument.", {
  testthat::expect_error(
    freq_study |> freq_tbl(),
    "A value for the '.freq_var' argument is missing with no default. freq_tbl\\() expects the value passed to the '.freq_var' argument \\(the second argument) to be a column of the data frame passed to '.data.'"
  )
})

# A column name that doesn't exist in .data is passed to .freq_var
# Return informative error
testthat::test_that("Error when a column name that doesn't exist in .data is passed to .freq_var.", {
  testthat::expect_error(
    freq_study |> freq_tbl(age),
    "freq_tbl\\() expects the value passed to the '.freq_var' argument \\(the second argument) to be a column of the data frame passed to the '.data.' argument. The value currently being passed to '.freq_var' is called 'age' and is not recognized as a column in the data frame being passed to '.data.'"
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

# -----------------------------------------------------------------------------
# One-way table - basic
# -----------------------------------------------------------------------------
one_way <- freq_study |> freq_tbl(sex)

testthat::test_that("The class of the one-way freq_tbl is as expected.", {
  testthat::expect_is(one_way, "tbl_df")
})

testthat::test_that("The dimensions of the one-way freq_tbl are as expected.", {
  testthat::expect_equal(dim(one_way), c(3L, 3L))
})

testthat::test_that("The column names of the one-way freq_tbl are as expected.", {
  testthat::expect_setequal(names(one_way), c("sex", "n", "prop"))
})

testthat::test_that("The values of .freq_var for the one-way freq_tbl are as expected", {
  testthat::expect_setequal(one_way[[1]], c("Female", "Male", NA))
})

testthat::test_that("The correct default statistics are returned for the one-way freq_tbl.", {
  testthat::expect_equal(one_way[[2]], c(57, 42, 1))
  testthat::expect_equal(one_way[[3]], c(0.57, 0.42, 0.01))
})

# -----------------------------------------------------------------------------
# One-way filter out missing
# -----------------------------------------------------------------------------
one_way_no_na <- freq_study |>
  dplyr::filter(!is.na(sex)) |>
  freq_tbl(sex)

testthat::test_that("The dimensions of the one-way freq_tbl without NA are as expected.", {
  testthat::expect_equal(dim(one_way_no_na), c(2L, 3L))
})

testthat::test_that("The values of .freq_var for the one-way freq_tbl without NA are as expected.", {
  testthat::expect_setequal(one_way_no_na[[1]], c("Female", "Male"))
})

testthat::test_that("The correct default statistics are returned for the one-way freq_tbl without NA.", {
  testthat::expect_setequal(one_way_no_na[[2]], c(57, 42))
  # Round or else slight differences in decimal values will cause the test to
  # fail. Round to something greater than 2 decimal places so that the values
  # will be different than they were for the basic one-way table above.
  testthat::expect_equal(one_way_no_na[[3]] |> round(3), c(0.576, 0.424))
})

# -----------------------------------------------------------------------------
# One-way table with .drop = TRUE
# Drop categories with zero observations.
# -----------------------------------------------------------------------------
one_way_drop_false <- freq_study |> freq_tbl(bmi_4cat, .drop = FALSE)
one_way_drop_true <- freq_study |> freq_tbl(bmi_4cat, .drop = TRUE)

testthat::test_that("The dimensions of the one-way freq_tbl with .drop = FALSE are as expected.", {
  testthat::expect_equal(dim(one_way_drop_false), c(6L, 3L))
})

testthat::test_that("The values of .freq_var for the one-way freq_tbl with .drop = FALSE are as expected.", {
  testthat::expect_setequal(
    one_way_drop_false[[1]],
    c("Underweight", "Normal", "Overweight", "Obese", "Extreme Obese", NA)
  )
})

testthat::test_that("The dimensions of the one-way freq_tbl with .drop = TRUE are as expected.", {
  testthat::expect_equal(dim(one_way_drop_true), c(5L, 3L))
})

testthat::test_that("The values of .freq_var for the one-way freq_tbl with .drop = TRUE are as expected.", {
  testthat::expect_setequal(
    one_way_drop_true[[1]],
    c("Underweight", "Normal", "Overweight", "Obese", NA)
  )
})

# -----------------------------------------------------------------------------
# One-way table with percentages instead of proportions
# -----------------------------------------------------------------------------
one_way_percent <- freq_study |> freq_tbl(sex, .percent = TRUE)

testthat::test_that("The column names of the one-way freq_tbl with .percent = TRUE are as expected.", {
  testthat::expect_setequal(
    names(one_way_percent),
    c("sex", "n", "percent")
  )
})

testthat::test_that("The .percent = TRUE argument works as expected for the one-way freq_tbl.", {
  testthat::expect_equal(one_way_percent[[3]], c(57, 42, 1))
})


# =============================================================================
# Test two-way frequency tables
# =============================================================================

# -----------------------------------------------------------------------------
# Two-way table - Basic
# -----------------------------------------------------------------------------
two_way <- freq_study |>
  dplyr::group_by(exposure) |>
  freq_tbl(sex)

testthat::test_that("The class of the two-way freq_tbl is as expected.", {
  testthat::expect_is(two_way, "tbl_df")
})

testthat::test_that("The dimensions of the two-way freq_tbl are as expected.", {
  testthat::expect_equal(dim(two_way), c(7L, 6L))
})

testthat::test_that("The column names of the two-way freq_tbl are as expected.", {
  testthat::expect_setequal(
    names(two_way),
    c("exposure", "sex", "n", "prop", "n_group", "prop_group")
  )
})

testthat::test_that("The values for the first group_by var in the two-way freq_tbl are as expected", {
  testthat::expect_setequal(
    two_way[[1]],
    c("No", "No", "No", "Yes", "Yes", NA)
  )
})

testthat::test_that("The values of .freq_var in the two-way freq_tbl are as expected", {
  testthat::expect_setequal(two_way[[2]], c("Female", "Male", NA, "Female", "Male", "Female", "Male"))
})

testthat::test_that("The correct default statistics are returned for the basic two-way freq_tbl.", {
  testthat::expect_equal(two_way[[3]], c(48, 35, 1, 8, 7, 1, 0))
  testthat::expect_equal(
    two_way[[4]],
    c(0.48, 0.35, 0.01, 0.08, 0.07, 0.01, 0)
  )
  testthat::expect_equal(two_way[[5]], c(84, 84, 84, 15, 15, 1, 1))
  testthat::expect_equal(
    two_way[[6]] |> round(3),
    c(0.571, 0.417, 0.012, 0.533, 0.467, 1, 0)
  )
})

# -----------------------------------------------------------------------------
# Two-way filter out missing
# -----------------------------------------------------------------------------
two_way_no_na <- freq_study |>
  dplyr::filter(!is.na(sex) & !is.na(exposure)) |>
  dplyr::group_by(exposure) |>
  freq_tbl(sex)

testthat::test_that("The dimensions of the two-way freq_tbl without NA are as expected.", {
  testthat::expect_equal(dim(two_way_no_na), c(4L, 6L))
})

testthat::test_that("The values for the first group_by var in the two-way freq_tbl without NA are as expected", {
  testthat::expect_setequal(two_way_no_na[[1]], c("No", "No", "Yes", "Yes"))
})

testthat::test_that("The values of .freq_var in the two-way freq_tbl without NA are as expected", {
  testthat::expect_setequal(
    two_way_no_na[[2]],
    c("Female", "Male", "Female", "Male")
  )
})

testthat::test_that("The correct default statistics are returned for the two-way freq_tbl without NA.", {
  testthat::expect_equal(two_way_no_na[[3]], c(48, 35, 8, 7))
  testthat::expect_equal(
    two_way_no_na[[4]] |> round(3),
    c(0.49, 0.357, 0.082, 0.071)
  )
  testthat::expect_equal(two_way_no_na[[5]], c(83, 83, 15, 15))
  testthat::expect_equal(
    two_way_no_na[[6]] |> round(3),
    c(0.578, 0.422, 0.533, 0.467)
  )
})

# -----------------------------------------------------------------------------
# Two-way table with .drop = TRUE
# Drop categories with zero observations
# -----------------------------------------------------------------------------
two_way_drop_false <- freq_study |>
  dplyr::group_by(exposure) |>
  freq_tbl(bmi_4cat, .drop = FALSE)

two_way_drop_true <- freq_study |>
  dplyr::group_by(exposure) |>
  freq_tbl(bmi_4cat, .drop = TRUE)

testthat::test_that("The dimensions of the two-way freq_tbl with .drop = FALSE are as expected.", {
  testthat::expect_equal(dim(two_way_drop_false), c(16L, 6L))
})

testthat::test_that("The values of .freq_var for the two-way freq_tbl with .drop = FALSE are as expected.", {
  testthat::expect_setequal(
    two_way_drop_false[[2]],
    c(
      "Underweight", "Normal", "Overweight", "Obese", "Extreme Obese", NA,
      "Underweight", "Normal", "Overweight", "Obese", "Extreme Obese",
      "Underweight", "Normal", "Overweight", "Obese", "Extreme Obese"
    )
  )
})

testthat::test_that("The dimensions of the two-way freq_tbl with .drop = TRUE are as expected.", {
  testthat::expect_equal(dim(two_way_drop_true), c(10L, 6L))
})

testthat::test_that("The values of .freq_var for the two-way freq_tbl with .drop = TRUE are as expected.", {
  testthat::expect_setequal(
    two_way_drop_true[[2]],
    c(
      "Underweight", "Normal", "Overweight", "Obese", NA, "Underweight", "Normal",
      "Overweight", "Obese", "Overweight"
    )
  )
})

# -----------------------------------------------------------------------------
# Two-way table with percentages instead of proportions
# -----------------------------------------------------------------------------
two_way_percent <- freq_study |>
  dplyr::group_by(exposure) |>
  freq_tbl(sex, .percent = TRUE)

testthat::test_that("The column names of the two-way freq_tbl with .percent = TRUE are as expected.", {
  testthat::expect_setequal(
    names(two_way_percent),
    c("exposure", "sex", "n", "percent", "n_group", "percent_group")
  )
})

testthat::test_that("The .percent = TRUE argument works as expected for the two-way freq_tbl.", {
  testthat::expect_equal(two_way_percent[[4]], c(48, 35, 1, 8, 7, 1, 0))
  testthat::expect_equal(
    two_way_percent[[6]] |> round(3),
    c(57.143, 41.667, 1.19, 53.333, 46.667, 100, 0)
  )
})


# =============================================================================
# Test N-way frequency tables
# =============================================================================

# -----------------------------------------------------------------------------
# N-way table - Basic
# -----------------------------------------------------------------------------
n_way <- freq_study |>
  dplyr::group_by(exposure, age_group) |>
  freq_tbl(sex)

testthat::test_that("The class of the N-way freq_tbl is as expected.", {
  testthat::expect_is(n_way, "tbl_df")
})

testthat::test_that("The dimensions of the N-way freq_tbl are as expected.", {
  testthat::expect_equal(dim(n_way), c(15L, 7L))
})

testthat::test_that("The column names of the N-way freq_tbl are as expected.", {
  testthat::expect_setequal(
    names(n_way),
    c("exposure", "age_group", "sex", "n", "prop", "n_group", "prop_group")
  )
})

testthat::test_that("The values for the first group_by var in the N-way freq_tbl are as expected", {
  testthat::expect_setequal(
    n_way[[1]],
    c(
      "No", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes", NA, NA, NA, NA,
      NA, NA
    )
  )
})

testthat::test_that("The values for the second group_by var in the N-way freq_tbl are as expected", {
  testthat::expect_setequal(
    n_way[[2]],
    c(
      "Younger than 30", "Younger than 30", "Younger than 30", "30 and Older",
      "30 and Older", "Younger than 30", "Younger than 30", "30 and Older",
      "30 and Older", "Younger than 30", "Younger than 30", "30 and Older",
      "30 and Older", NA, NA
    )
  )
})

testthat::test_that("The values of .freq_var in the N-way freq_tbl are as expected", {
  testthat::expect_setequal(
    n_way[[3]],
    c(
      "Female", "Male", NA, "Female", "Male", "Female", "Male", "Female",
      "Male", "Female", "Male", "Female", "Male", "Female", "Male"
    )
  )
})

testthat::test_that("The correct default statistics are returned for the basic N-way freq_tbl.", {
  testthat::expect_equal(
    n_way[[4]],
    c(33, 20, 1, 15, 15, 4, 2, 4, 5, 0, 0, 0, 0, 1, 0)
  )
  testthat::expect_equal(
    n_way[[5]],
    c(0.33, 0.2, 0.01, 0.15, 0.15, 0.04, 0.02, 0.04, 0.05, 0, 0, 0, 0, 0.01, 0)
  )
  testthat::expect_equal(
    n_way[[6]],
    c(54, 54, 54, 30, 30, 6, 6, 9, 9, 0, 0, 0, 0, 1, 1)
  )
  testthat::expect_equal(
    n_way[[7]] |> round(3),
    c(
      0.611, 0.37, 0.019, 0.5, 0.5, 0.667, 0.333, 0.444, 0.556, NaN, NaN, NaN,
      NaN, 1, 0
    )
  )
})

# -----------------------------------------------------------------------------
# N-way filter out missing
# -----------------------------------------------------------------------------
n_way_no_na <- freq_study |>
  dplyr::filter(!is.na(sex) & !is.na(exposure) & !is.na(age_group)) |>
  dplyr::group_by(exposure, age_group) |> freq_tbl(sex)

testthat::test_that("The dimensions of the N-way freq_tbl without NA are as expected.", {
  testthat::expect_equal(dim(n_way_no_na), c(8L, 7L))
})

testthat::test_that("The values for the first group_by var in the N-way freq_tbl without NA are as expected", {
  testthat::expect_setequal(
    n_way_no_na[[1]],
    c("No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes")
  )
})

testthat::test_that("The values for the second group_by var in the N-way freq_tbl without NA are as expected", {
  testthat::expect_setequal(
    n_way_no_na[[2]],
    c(
      "Younger than 30", "Younger than 30", "30 and Older", "30 and Older",
      "Younger than 30", "Younger than 30", "30 and Older", "30 and Older"
    )
  )
})

testthat::test_that("The values of .freq_var in the N-way freq_tbl without NA are as expected", {
  testthat::expect_setequal(
    n_way_no_na[[3]],
    c("Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male")
  )
})

testthat::test_that("The correct default statistics are returned for the N-way freq_tbl without NA.", {
  testthat::expect_equal(n_way_no_na[[4]], c(33, 20, 15, 15, 4, 2, 4, 5))
  testthat::expect_equal(
    n_way_no_na[[5]] |> round(3),
    c(0.337, 0.204, 0.153, 0.153, 0.041, 0.02, 0.041, 0.051)
  )
  testthat::expect_equal(n_way_no_na[[6]], c(53, 53, 30, 30, 6, 6, 9, 9))
  testthat::expect_equal(
    n_way_no_na[[7]] |> round(3),
    c(0.623, 0.377, 0.5, 0.5, 0.667, 0.333, 0.444, 0.556)
  )
})

# -----------------------------------------------------------------------------
# Two-way table with .drop = TRUE
# Drop categories with zero observations
# -----------------------------------------------------------------------------
n_way_drop_false <- freq_study |>
  dplyr::group_by(exposure, age_group) |>
  freq_tbl(bmi_4cat, .drop = FALSE)

n_way_drop_true <- freq_study |>
  dplyr::group_by(exposure, age_group) |>
  freq_tbl(bmi_4cat, .drop = TRUE)

testthat::test_that("The dimensions of the N-way freq_tbl with .drop = FALSE are as expected.", {
  testthat::expect_equal(dim(n_way_drop_false), c(37L, 7L))
})

testthat::test_that("The values of .freq_var for the N-way freq_tbl with .drop = FALSE are as expected.", {
  testthat::expect_setequal(
    n_way_drop_false[[3]],
    c(
      "Underweight", "Normal", "Overweight", "Obese", "Extreme Obese", NA,
      "Underweight", "Normal", "Overweight", "Obese", "Extreme Obese", NA,
      "Underweight", "Normal", "Overweight", "Obese", "Extreme Obese",
      "Underweight", "Normal", "Overweight", "Obese", "Extreme Obese",
      "Underweight", "Normal", "Overweight", "Obese", "Extreme Obese",
      "Underweight", "Normal", "Overweight", "Obese", "Extreme Obese",
      "Underweight", "Normal", "Overweight", "Obese", "Extreme Obese"
    )
  )
})

testthat::test_that("The dimensions of the N-way freq_tbl with .drop = TRUE are as expected.", {
  testthat::expect_equal(dim(n_way_drop_true), c(18L, 7L))
})

testthat::test_that("The values of .freq_var for the N-way freq_tbl with .drop = TRUE are as expected.", {
  testthat::expect_setequal(
    n_way_drop_true[[3]],
    c(
      "Underweight", "Normal", "Overweight", "Obese", NA, "Underweight",
      "Normal", "Overweight", "Obese", NA, "Normal", "Overweight", "Obese",
      "Underweight", "Normal", "Overweight", "Obese", "Overweight"
    )
  )
})

# -----------------------------------------------------------------------------
# N-way table with percentages instead of proportions
# -----------------------------------------------------------------------------
n_way_percent <- freq_study |>
  dplyr::group_by(exposure, age_group) |>
  freq_tbl(sex, .percent = TRUE)

testthat::test_that("The column names of the N-way freq_tbl with .percent = TRUE are as expected.", {
  testthat::expect_setequal(
    names(n_way_percent),
    c("exposure", "age_group", "sex", "n", "percent", "n_group", "percent_group")
  )
})

testthat::test_that("The .percent = TRUE argument works as expected for the N-way freq_tbl.", {
  testthat::expect_equal(
    n_way_percent[[5]],
    c(33, 20, 1, 15, 15, 4, 2, 4, 5, 0, 0, 0, 0, 1, 0)
  )
  testthat::expect_equal(
    n_way_percent[[7]] |> round(3),
    c(
      61.111, 37.037, 1.852, 50, 50, 66.667, 33.333, 44.444, 55.556, NaN, NaN,
      NaN, NaN, 100, 0
    )
  )
})
