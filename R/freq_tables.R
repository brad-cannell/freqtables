#' @title Create Multiple Frequency Tables at Once
#'
#' @description The freq_tables function (with an "s") allows the user to pass
#'   more than one outcome variable. It returns a nested data frame with one
#'   row per variable and a list column containing the individual frequency
#'   tables.
#'
#' @param .data A data frame or grouped data frame.
#'
#' @param ... One or more unquoted column names to tabulate.
#'
#' @param percent_ci The confidence level as a percentage. Default is 95.
#'
#' @param ci_type Method for confidence interval estimation. Either "logit"
#'   (default) or "wald".
#'
#' @param drop If FALSE (default), unobserved factor levels will appear in
#'   the results with n = 0.
#'
#' @return A tibble with columns \code{col} (the variable name) and
#'   \code{freq_table} (a list column of frequency table data frames).
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(freqtables)
#'
#' data(mtcars)
#'
#' # Multiple one-way tables
#' mtcars %>%
#'   freq_tables(am, cyl)
#'
#' # Multiple grouped tables
#' mtcars %>%
#'   group_by(vs) %>%
#'   freq_tables(am, cyl)
freq_tables <- function(.data, ..., percent_ci = 95, ci_type = "logit",
                        drop = FALSE) {

  # ===========================================================================
  # Check that .data is a data frame
  # ===========================================================================
  if (!is.data.frame(.data)) {
    stop(
      "freq_tables expects a data frame. The object passed has class: ",
      paste(class(.data), collapse = ", "), "."
    )
  }

  # ===========================================================================
  # Capture the column names
  # ===========================================================================
  col_quos <- rlang::enquos(...)

  if (length(col_quos) == 0) {
    stop("No column names were passed to freq_tables().")
  }

  col_names <- vapply(col_quos, rlang::as_name, character(1))

  # ===========================================================================
  # Build a frequency table for each column
  # ===========================================================================
  tables <- lapply(col_names, function(cn) {
    rlang::inject(
      freq_table(.data, !!rlang::sym(cn), percent_ci = percent_ci,
                 ci_type = ci_type, drop = drop)
    )
  })

  # ===========================================================================
  # Return a nested data frame
  # ===========================================================================
  dplyr::tibble(
    col = col_names,
    freq_table = tables
  )
}
