#' Frequency tables with confidence intervals
#'
#' Combine [freq_tbl()] with [logit_ci()] or [wald_ci()]. Accept one outcome;
#' supply groups using [dplyr::group_by()]. Defaults are compact proportions
#' with confidence intervals. See `vignette("migration")` for the transition
#' from the old percentage-scale, two-variable interface.
#'
#' @inheritParams freq_tbl
#' @inheritParams confidence_intervals
#' @param ci_type Interval method: `"logit"` (default) or `"wald"`.
#'   Both methods support ungrouped and grouped data. Supply a string.
#' @return An ungrouped tibble with the [freq_tbl()] columns plus confidence
#'   limits. Classes are `freq_table_one_way` (no groups),
#'   `freq_table_two_way` (one group), or `freq_table_n_way` (multiple
#'   groups), together with `freq_tbl` and tibble classes.
#'   [freq_test()] supports the first two cases; multiple grouping variables
#'   do not imply a particular statistical test.
#' @export
#' @examples
#' freq_table(mtcars, am)
#' mtcars |> dplyr::group_by(cyl) |> freq_table(am)
#' mtcars |> dplyr::group_by(cyl, vs) |>
#'   freq_table(am, ci_type = "wald", percent_ci = 90)
#' freq_table(mtcars, am, percent = TRUE, overall = TRUE,
#'            se = TRUE, critical_value = TRUE, generic_col_names = TRUE)
freq_table <- function(.data, .freq_var, ..., percent_ci = 95,
                       ci_type = "logit", drop = FALSE, percent = FALSE,
                       overall = FALSE, se = FALSE, critical_value = FALSE,
                       generic_col_names = FALSE) {
  check_frequency_input(.data, rlang::enquo(.freq_var), rlang::enquos(...),
                        "freq_table")
  if (!is.character(ci_type) || length(ci_type) != 1L ||
      is.na(ci_type) || !ci_type %in% c("logit", "wald")) {
    stop('ci_type must be "logit" or "wald".', call. = FALSE)
  }
  out <- freq_tbl(.data, {{ .freq_var }}, drop = drop, percent = percent,
                  overall = overall, generic_col_names = generic_col_names)
  helper <- if (ci_type == "wald") wald_ci else logit_ci
  out <- helper(out, percent_ci = percent_ci, se = se,
                 critical_value = critical_value)
  n_groups <- length(dplyr::group_vars(.data))
  table_class <- if (n_groups == 0L) "freq_table_one_way" else
    if (n_groups == 1L) "freq_table_two_way" else "freq_table_n_way"
  class(out) <- c(table_class, class(out))
  out
}
