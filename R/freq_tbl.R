freq_tbl <- function(.data, .freq_var, .drop = FALSE, .percent = FALSE) {

  # ===========================================================================
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ===========================================================================
  # Fill this in later.

  # ===========================================================================
  # Setup for tidy evaluation
  # ===========================================================================
  freq_var <- rlang::enquo(.freq_var)

  # ===========================================================================
  # Data Checks
  # ===========================================================================

  # ---------------------------------------------------------------------------
  # Check the values being passed to freq_tbl()
  # ---------------------------------------------------------------------------
  check_freq_var <- deparse(substitute(.freq_var))
  check_drop     <- deparse(substitute(.drop))
  check_percent  <- deparse(substitute(.percent))

  ## Check to see what type of object is being passed to .data
  ##   - Should be a data frame.
  ##   - For example, some people use "attach". That won't work with freq_tbl()
  .data_class <- class(.data)
  if (!("data.frame" %in% .data_class)) {
    stop(
      paste0(
        "freq_tbl expects the object passed to the .data argument (the",
        " first argument) to be a data frame. Currently, the object being",
        " passed to .data has the class: ", .data_class, ". Please use the",
        " form mtcars |> freq_table(am) or freq_table(mtcars, am)."
      )
    )
  }

  ## Check to make sure the value passed to .freq_var is a column of .data.            Left off here... Working on informative error when nothing is passed to freq_tbl()
  return(freq_var)
  freq_var_in_data <- check_freq_var %in% names(.data)
  # return(freq_var_in_data)

  # ---------------------------------------------------------------------------
  # Check to make sure there was a value passed to the .freq_var argument.
  #   - Not necessary. If it is missing then 'argument ".freq_var" is missing,
  #     with no default' is thrown automatically.
  # ---------------------------------------------------------------------------

  # ---------------------------------------------------------------------------
  # Check to make sure that .freq_var is a column that exists in .data.
  # ---------------------------------------------------------------------------
  freq_var_chr <- rlang::as_name(freq_var)
  col_names <- names(.data)
  if (!(freq_var_chr %in% col_names)) {
    stop(
      paste0(
        "freq_tbl expects the object passed to the .freq_var argument (the",
        " second argument) to be a column of the data frame passed to .data.",
        " The object being passed to .freq_var is called '", .freq_var_chr,
        "' and is not a column in the data frame being passed to .data."
      )
    )
  }

  # ===========================================================================
  # One-way frequency table
  # ===========================================================================

  # Get n overall
  n_overall <- nrow(.data)

  # Get counts and proportions
  out <- .data |>
    dplyr::count({{ .freq_var }}, .drop = .drop) |>
    # Add proportions
    dplyr::mutate(prop = n / n_overall)

  # Convert proportion to percentage (optional)
  if (.percent) {
    out <- dplyr::mutate(out, percent = prop * 100, prop = NULL)
  }

  # ===========================================================================
  # N-way frequency table
  # If creating an n-way frequency table, add n_group and prop_group (or
  # percent_group).
  # ===========================================================================

  # Add n_group and prop_group
  if ("grouped_df" %in% class(.data)) {
    out <- out |>
      dplyr::mutate(
        n_group    = sum(n),
        prop_group = n / sum(n),

      )

    # Convert group proportion to percentage (optional)
    # Grouped tibbles only
    if (.percent) {
      out <- out |>
        dplyr::mutate(
          percent_group = prop_group * 100,
          prop_group    = NULL
        )
    }
  }

  # Return tibble of results
  out
}
