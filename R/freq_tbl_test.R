# Testing the freq_tbl function across different scenarios
# Add this to build ignore. It's just for use during development.

freq_tbl_test <- function() {

  # Error checks --------------------------------------------------------------

  # Not a data frame
  # Return informative error
  not_df <- testthat::capture_error(freq_study$sex |> freq_tbl())

  # Nothing passed to .freq_var
  # Return informative error
  freq_var_empty <- testthat::capture_error(freq_study |> freq_tbl())

  # A column name that doesn't exist in .data is passed to .freq_var
  # Return informative error
  col_not_in_data <- testthat::capture_error(freq_study |> freq_tbl(age))

  # Passing more than one column to freq_tbl
  one_way_mult_col <- testthat::capture_error(freq_study |> freq_tbl(exposure, sex))

  # One-way tables ------------------------------------------------------------

  # One-way table - basic
  one_way <- freq_study |> freq_tbl(sex)

  # One-way filter out missing
  one_way_no_na <- freq_study |> dplyr::filter(!is.na(sex)) |> freq_tbl(sex)

  # One-way table with .drop = TRUE
  # Drop categories with zero observations?
  one_way_drop_false <- freq_study |> freq_tbl(bmi_4cat, .drop = FALSE)
  one_way_drop_true <- freq_study |> freq_tbl(bmi_4cat, .drop = TRUE)

  # One-way table with percentages instead of proportions
  one_way_percent <- freq_study |> freq_tbl(sex, .percent = TRUE)

  # Two-way tables ------------------------------------------------------------

  # Two-way table - Basic
  two_way <- freq_study |> dplyr::group_by(exposure) |> freq_tbl(sex)

  # Two-way filter out missing
  two_way_no_na <- freq_study |> dplyr::filter(!is.na(sex) & !is.na(exposure)) |> dplyr::group_by(exposure) |> freq_tbl(sex)

  # Two-way table with .drop = TRUE
  # Drop categories with zero observations?
  two_way_drop_false <- freq_study |> dplyr::group_by(exposure) |> freq_tbl(bmi_4cat, .drop = FALSE)
  two_way_drop_true <- freq_study |> dplyr::group_by(exposure) |> freq_tbl(bmi_4cat, .drop = TRUE)

  # Two-way table with percentages instead of proportions
  two_way_percent <- freq_study |> dplyr::group_by(exposure) |> freq_tbl(sex, .percent = TRUE)

  # N-way tables --------------------------------------------------------------

  # N-way table - Basic
  n_way <- freq_study |> dplyr::group_by(exposure, age_group) |> freq_tbl(sex)

  # N-way filter out missing
  n_way_no_na <- freq_study |> dplyr::filter(!is.na(sex) & !is.na(exposure) & !is.na(age_group)) |> dplyr::group_by(exposure, age_group) |> freq_tbl(sex)


  # Put all the results into a list -------------------------------------------
  results <- list(
    # Errors
    "Not a data frame" = not_df,
    "Nothing passed to .freq_var" = freq_var_empty,
    "A column name that doesn't exist in .data is passed to .freq_var" = col_not_in_data,
    "Passing more than one column to freq_tbl" = one_way_mult_col,
    # One-way tables
    "One-way table - basic" = one_way,
    "One-way filter out missing" = one_way_no_na,
    "One-way table with .drop = FALSE" = one_way_drop_false,
    "One-way table with .drop = TRUE" = one_way_drop_true,
    "One-way table with percentages instead of proportions" = one_way_percent,
    # Two-way tables
    "Two-way table - Basic" = two_way,
    "Two-way filter out missing" = two_way_no_na,
    "Two-way table with .drop = FALSE" = two_way_drop_false,
    "Two-way table with .drop = TRUE" = two_way_drop_true,
    "Two-way table with percentages instead of proportions" = two_way_percent,
    # N-way tables
    "N-way table - Basic" = n_way,
    "N-way filter out missing" = n_way_no_na
  )

  # Return test results
  results
}
