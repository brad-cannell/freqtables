#' @title Calculate Wald Confidence Intervals
#'
#' @description Calculates Wald (linear) confidence intervals for proportions.
#'   Designed to be used in a pipeline after \code{freq_tbl()}.
#'
#'   The standard error is calculated as:
#'   \code{sqrt(proportion * (1 - proportion) / (n_denominator - 1))}
#'
#'   The confidence interval is calculated as:
#'   \code{proportion +/- t_crit * se}
#'
#'   where t_crit is the critical value from Student's t distribution.
#'
#' @param .data A data frame, typically output from \code{freq_tbl()}.
#'
#' @param percent_ci The confidence level as a percentage. Default is 95 for
#'   95\% confidence intervals.
#'
#' @return A tibble with additional columns for the standard error (se),
#'   critical value (t_crit), lower confidence limit (lcl), and upper
#'   confidence limit (ucl). For grouped data, the columns are suffixed with
#'   _group.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(freqtables)
#'
#' data(mtcars)
#'
#' # Wald CI for a one-way table
#' mtcars %>%
#'   freq_tbl(am) %>%
#'   wald_ci()
#'
#' # Wald CI for a grouped table
#' mtcars %>%
#'   group_by(cyl) %>%
#'   freq_tbl(am) %>%
#'   wald_ci()
wald_ci <- function(.data, percent_ci = 95) {

  # Prevents R CMD check
  prop <- prop_group <- n_group <- se <- se_group <- t_crit <- t_crit_group <- NULL
  lcl <- ucl <- lcl_group <- ucl_group <- n <- NULL

  # ===========================================================================
  # Check that .data is a data frame
  # ===========================================================================
  if (!is.data.frame(.data)) {
    stop(
      "wald_ci expects a data frame. The object passed has class: ",
      paste(class(.data), collapse = ", "), "."
    )
  }

  # ===========================================================================
  # Calculate alpha and t_prob
  # ===========================================================================
  alpha <- 1 - (percent_ci / 100)
  t_prob <- 1 - alpha / 2

  # ===========================================================================
  # Determine if grouped (has prop_group column) or ungrouped (has prop column)
  # ===========================================================================
  if ("prop_group" %in% names(.data)) {
    # Grouped data
    out <- .data %>%
      dplyr::mutate(
        se_group     = sqrt(prop_group * (1 - prop_group) / (n_group - 1)),
        t_crit_group = stats::qt(t_prob, df = n_group - 1),
        lcl_group    = prop_group - t_crit_group * se_group,
        ucl_group    = prop_group + t_crit_group * se_group
      )
  } else if ("prop" %in% names(.data)) {
    # Ungrouped data
    n_total <- sum(.data$n)
    out <- .data %>%
      dplyr::mutate(
        se     = sqrt(prop * (1 - prop) / (n_total - 1)),
        t_crit = stats::qt(t_prob, df = n_total - 1),
        lcl    = prop - t_crit * se,
        ucl    = prop + t_crit * se
      )
  } else {
    stop(
      "wald_ci expects a data frame with a 'prop' or 'prop_group' column. ",
      "Did you run freq_tbl() first?"
    )
  }

  out
}
