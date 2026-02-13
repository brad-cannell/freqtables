#' Frequency table with confidence intervals
#'
#' @description
#' `freq_table()` is a higher-level wrapper around `freq_tbl()` that adds
#' confidence intervals by default.
#'
#' @param .data A data frame (optionally grouped with dplyr::group_by()).
#' @param ... A single categorical outcome variable.
#' @param percent_ci Confidence level as a percent (default 95).
#' @param ci_type Either "logit" (default) or "wald".
#' @param drop Passed to dplyr::count(.drop = ).
#' @param percent If TRUE, express proportion-related columns as percentages.
#' @param overall If TRUE, include overall totals/proportions.
#' @param generic_col_names If TRUE, return generic variable/category columns.
#' @param se If TRUE, retain standard error columns in output.
#' @param critical_value If TRUE, retain critical value columns in output.
#' @return A tibble.
#' @export
freq_table <- function(.data,
                       ...,
                       percent_ci = 95,
                       ci_type = "logit",
                       drop = FALSE,
                       percent = FALSE,
                       overall = FALSE,
                       generic_col_names = FALSE,
                       se = FALSE,
                       critical_value = FALSE) {

  outcome_quos <- rlang::enquos(...)
  if (length(outcome_quos) > 1) {
    stop(
      "Passing more than one column name to freq_table() is deprecated. Use dplyr::group_by() for grouped analyses, e.g. `mtcars |> dplyr::group_by(cyl) |> freq_table(am)` instead of `mtcars |> freq_table(cyl, am)`."
    )
  }
  if (length(outcome_quos) < 1) {
    stop("Did you pass a column name to `...`? Example: mtcars |> freq_table(am).")
  }

  out <- freq_tbl(
    .data = .data,
    ...,
    percent = FALSE,
    overall = overall,
    generic_col_names = generic_col_names,
    drop = drop
  )

  if (identical(ci_type, "wald")) {
    out <- wald_ci(out, percent_ci = percent_ci)
  } else if (identical(ci_type, "logit")) {
    out <- logit_ci(out, percent_ci = percent_ci)
  } else {
    stop("ci_type must be either 'logit' or 'wald'.")
  }

  # Slim default output
  drop_cols <- character(0)
  if (!se) {
    drop_cols <- c(drop_cols, grep("^se", names(out), value = TRUE))
  }
  if (!critical_value) {
    drop_cols <- c(drop_cols, grep("^t_crit", names(out), value = TRUE))
  }
  out <- dplyr::select(out, -dplyr::any_of(unique(drop_cols)))

  if (percent) {
    prop_cols <- names(out)[grepl("^prop|^lcl|^ucl", names(out))]
    out <- dplyr::mutate(out, dplyr::across(dplyr::all_of(prop_cols), ~ .x * 100))
  }

  group_n <- length(dplyr::group_vars(.data))
  if (group_n == 0) {
    class(out) <- c("freq_table_one_way", class(out))
  } else if (group_n == 1) {
    class(out) <- c("freq_table_two_way", class(out))
  } else {
    class(out) <- c("freq_table_grouped", class(out))
  }

  out
}
