freq_tbl <- function(.data, .freq_var, .drop = FALSE, .percent = FALSE) {

  # ===========================================================================
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ===========================================================================
  # Fill this in later.

  # ===========================================================================
  # Data Checks
  # ===========================================================================

  # ---------------------------------------------------------------------------
  # Check to see what type of object is being passed to .data
  #   - Should be a data frame.
  #   - For example, some people use "attach". That won't work with freq_tbl()
  # ---------------------------------------------------------------------------
  data_class <- class(.data)
  if (!("data.frame" %in% data_class)) {
    stop(glue::glue(
      "freq_tbl() expects the value passed to the '.data' argument (the first",
      " argument) to be a data frame. Currently, the value being passed to",
      " '.data' has the class: '{data_class}'. Please use the form 'mtcars |>",
      " freq_tbl(am)' or 'freq_tbl(mtcars, am)'."
    ))
  }

  # ---------------------------------------------------------------------------
  # Check to make sure there was a value passed to the .freq_var argument.
  # ---------------------------------------------------------------------------
  freq_var_chr <- deparse(substitute(.freq_var))
  if (freq_var_chr == "") {
    stop(glue::glue(
      "A value for the '.freq_var' argument is missing with no default. ",
      "freq_tbl() expects the value passed to the '.freq_var' argument (the",
      " second argument) to be a column of the data frame passed to '.data.'"
    ))
  }

  # ---------------------------------------------------------------------------
  # Check to make sure that .freq_var is a column that exists in .data.
  # ---------------------------------------------------------------------------
  col_names <- names(.data)
  if (!(freq_var_chr %in% col_names)) {
    stop(glue::glue(
      "freq_tbl() expects the value passed to the '.freq_var' argument (the",
      " second argument) to be a column of the data frame passed to the",
      " '.data.' argument.",
      " The value currently being passed to '.freq_var' is called '{freq_var_chr}'",
      " and is not recognized as a column in the data frame being passed to",
      " '.data.'"
    ))
  }

  # ---------------------------------------------------------------------------
  # Check to make sure that the user is not trying to pass more than one
  # column name to .freq_var.
  # ---------------------------------------------------------------------------
  drop_chr <- deparse(substitute(.drop))

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
