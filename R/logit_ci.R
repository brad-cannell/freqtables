#' @title Calculate Logit-Transformed Confidence Intervals
#'
#' @description Calculates logit-transformed confidence intervals for
#'   proportions. This method produces confidence intervals equivalent to those
#'   used by Stata. Designed to be used in a pipeline after \code{freq_tbl()}.
#'
#'   The standard error is calculated as:
#'   \code{sqrt(proportion * (1 - proportion) / (n_denominator - 1))}
#'
#'   The logit transformation is applied as:
#'   \code{logit(p) = log(p) - log(1 - p)}
#'   \code{se_logit = se / (p * (1 - p))}
#'   \code{CI_logit = logit(p) +/- t_crit * se_logit}
#'   \code{CI = exp(CI_logit) / (1 + exp(CI_logit))}
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
#' # Logit CI for a one-way table
#' mtcars %>%
#'   freq_tbl(am) %>%
#'   logit_ci()
#'
#' # Logit CI for a grouped table
#' mtcars %>%
#'   group_by(cyl) %>%
#'   freq_tbl(am) %>%
#'   logit_ci()
logit_ci <- function(.data, percent_ci = 95) {

  # Prevents R CMD check
  prop <- prop_group <- n_group <- se <- se_group <- t_crit <- t_crit_group <- NULL
  lcl <- ucl <- lcl_group <- ucl_group <- n <- NULL
  prop_log <- se_log <- lcl_log <- ucl_log <- NULL
  prop_log_group <- se_log_group <- lcl_log_group <- ucl_log_group <- NULL

  # ===========================================================================
  # Check that .data is a data frame
  # ===========================================================================
  if (!is.data.frame(.data)) {
    stop(
      "logit_ci expects a data frame. The object passed has class: ",
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
        se_group        = sqrt(prop_group * (1 - prop_group) / (n_group - 1)),
        t_crit_group    = stats::qt(t_prob, df = n_group - 1),
        prop_log_group  = log(prop_group) - log(1 - prop_group),
        se_log_group    = se_group / (prop_group * (1 - prop_group)),
        lcl_log_group   = prop_log_group - t_crit_group * se_log_group,
        ucl_log_group   = prop_log_group + t_crit_group * se_log_group,
        lcl_group       = exp(lcl_log_group) / (1 + exp(lcl_log_group)),
        ucl_group       = exp(ucl_log_group) / (1 + exp(ucl_log_group))
      ) %>%
      dplyr::select(-prop_log_group, -se_log_group, -lcl_log_group, -ucl_log_group)

  } else if ("prop" %in% names(.data)) {
    # Ungrouped data
    n_total <- sum(.data$n)
    out <- .data %>%
      dplyr::mutate(
        se       = sqrt(prop * (1 - prop) / (n_total - 1)),
        t_crit   = stats::qt(t_prob, df = n_total - 1),
        prop_log = log(prop) - log(1 - prop),
        se_log   = se / (prop * (1 - prop)),
        lcl_log  = prop_log - t_crit * se_log,
        ucl_log  = prop_log + t_crit * se_log,
        lcl      = exp(lcl_log) / (1 + exp(lcl_log)),
        ucl      = exp(ucl_log) / (1 + exp(ucl_log))
      ) %>%
      dplyr::select(-prop_log, -se_log, -lcl_log, -ucl_log)

  } else {
    stop(
      "logit_ci expects a data frame with a 'prop' or 'prop_group' column. ",
      "Did you run freq_tbl() first?"
    )
  }

  out
}
