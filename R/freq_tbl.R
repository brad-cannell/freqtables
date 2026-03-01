#' @title Calculate Counts and Proportions in dplyr Pipelines
#'
#' @description The freq_tbl function produces frequency tables with counts and
#'   proportions for a single categorical variable. It is designed to work with
#'   \code{dplyr::group_by()} for grouped analyses.
#'
#'   For ungrouped data, freq_tbl returns the count and proportion for each
#'   level of the variable.
#'
#'   For grouped data (using \code{dplyr::group_by()}), freq_tbl returns the
#'   count, group size, and within-group proportion.
#'
#' @param .data A data frame or grouped data frame.
#'
#' @param .col A single unquoted column name for the categorical variable to
#'   tabulate.
#'
#' @param drop If FALSE (default), unobserved factor levels will be included in
#'   the returned frequency table with an n of 0.
#'
#' @return A tibble with class "freq_tbl". For ungrouped data, the tibble
#'   contains columns for the variable values, n, and prop. For grouped data,
#'   it contains the grouping variable columns, the variable values, n,
#'   n_group, and prop_group.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(freqtables)
#'
#' data(mtcars)
#'
#' # One-way frequency table
#' mtcars %>%
#'   freq_tbl(am)
#'
#' # Two-way frequency table using group_by
#' mtcars %>%
#'   group_by(cyl) %>%
#'   freq_tbl(am)
#'
#' # N-way frequency table using group_by with multiple variables
#' mtcars %>%
#'   group_by(cyl, vs) %>%
#'   freq_tbl(am)
freq_tbl <- function(.data, .col, drop = FALSE) {

  # Prevents R CMD check: "no visible binding for global variable '.'"
  n <- n_group <- NULL

  # ===========================================================================
  # Check that .data is a data frame
  # ===========================================================================
  if (!is.data.frame(.data)) {
    stop(
      "freq_tbl expects a data frame to be passed to the .data argument. ",
      "The object passed has class: ", paste(class(.data), collapse = ", "), "."
    )
  }

  # ===========================================================================
  # Capture the column argument
  # ===========================================================================
  col_quo <- rlang::enquo(.col)
  col_name <- rlang::as_name(col_quo)

  # ===========================================================================
  # Check that only one column is passed
  # ===========================================================================
  # If the user tries to pass multiple columns using freq_tbl(sex, diabetes),
  # the second argument would be captured by `drop`. We check for this by

  # seeing if drop is not logical.
  if (!is.logical(drop)) {
    stop(
      "freq_tbl() accepts only one column name. If you want a grouped ",
      "analysis, use dplyr::group_by() first. For example:\n",
      "  df %>% group_by(sex) %>% freq_tbl(diabetes)"
    )
  }

  # ===========================================================================
  # Detect grouping
  # ===========================================================================
  group_vars <- dplyr::group_vars(.data)
  is_grouped <- length(group_vars) > 0

  # ===========================================================================
  # Calculate counts
  # ===========================================================================
  if (is_grouped) {
    # Grouped analysis
    out <- dplyr::count(.data, !!col_quo, .drop = drop)

    # Calculate group totals and proportions
    out <- out %>%
      dplyr::mutate(
        n_group = sum(n),
        prop_group = n / n_group
      ) %>%
      dplyr::ungroup()

    # Add class
    class(out) <- c("freq_tbl", class(out))

  } else {
    # Ungrouped analysis
    out <- dplyr::count(.data, !!col_quo, .drop = drop)

    # Calculate proportions
    out <- out %>%
      dplyr::mutate(
        prop = n / sum(n)
      )

    # Add class
    class(out) <- c("freq_tbl", class(out))
  }

  out
}
