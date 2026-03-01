#' @title Create Contingency Tables (Cross-Tabulations)
#'
#' @description The freq_xtab function creates a contingency table (matrix)
#'   from two categorical variables. With \code{dplyr::group_by()}, it creates
#'   an array of contingency tables (one per group level).
#'
#' @param .data A data frame or grouped data frame.
#'
#' @param .row An unquoted column name for the row variable (e.g., exposure).
#'
#' @param .col An unquoted column name for the column variable (e.g., outcome).
#'
#' @param margins If TRUE (default), marginal totals ("Sum") are appended to
#'   the rows and columns of the table.
#'
#' @return For ungrouped data, a matrix with row and column names set to the
#'   variable levels, with marginal totals if \code{margins = TRUE}. For
#'   grouped data, a named list of such matrices (one per group level).
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(freqtables)
#'
#' data(mtcars)
#'
#' # Simple contingency table
#' mtcars %>%
#'   freq_xtab(am, cyl)
#'
#' # Grouped contingency tables
#' mtcars %>%
#'   group_by(vs) %>%
#'   freq_xtab(am, cyl)
freq_xtab <- function(.data, .row, .col, margins = TRUE) {

  # ===========================================================================
  # Check that .data is a data frame
  # ===========================================================================
  if (!is.data.frame(.data)) {
    stop(
      "freq_xtab expects a data frame. The object passed has class: ",
      paste(class(.data), collapse = ", "), "."
    )
  }

  # ===========================================================================
  # Capture arguments
  # ===========================================================================
  row_quo <- rlang::enquo(.row)
  col_quo <- rlang::enquo(.col)
  row_name <- rlang::as_name(row_quo)
  col_name <- rlang::as_name(col_quo)

  # ===========================================================================
  # Detect grouping
  # ===========================================================================
  group_vars <- dplyr::group_vars(.data)
  is_grouped <- length(group_vars) > 0

  # ===========================================================================
  # Helper to build one contingency table from a data frame
  # ===========================================================================
  build_xtab <- function(df) {
    ct <- table(df[[row_name]], df[[col_name]])
    dimnames(ct) <- stats::setNames(dimnames(ct), c(row_name, col_name))

    if (margins) {
      ct <- addmargins(ct)
    }

    ct
  }

  # ===========================================================================
  # Build the contingency table(s)
  # ===========================================================================
  if (is_grouped) {
    # Split by group and build one table per group
    .data_ungrouped <- dplyr::ungroup(.data)
    split_data <- split(.data_ungrouped, .data_ungrouped[group_vars])

    result <- lapply(split_data, build_xtab)

    # Clean up names if only one grouping variable
    if (length(group_vars) == 1) {
      names(result) <- paste0(group_vars, " = ", names(result))
    }

    result
  } else {
    build_xtab(.data)
  }
}
