#' Add Wald confidence intervals to freq_tbl/freq_table output
#'
#' @param .data A tibble containing `n` and either (`n_group`, `prop_group`) or
#'   (`n_total`, `prop`) columns.
#' @param percent_ci Confidence level as a percent (e.g., 95).
#' @return A tibble with lower/upper confidence limits.
#' @export
wald_ci <- function(.data, percent_ci = 95) {
  alpha <- 1 - (percent_ci / 100)
  t_prob <- 1 - alpha / 2

  if (all(c("n_group", "prop_group") %in% names(.data))) {
    .data |>
      dplyr::mutate(
        se_group = sqrt(prop_group * (1 - prop_group) / (n_group - 1)),
        t_crit_group = stats::qt(t_prob, df = n_group - 1),
        lcl_group = prop_group - t_crit_group * se_group,
        ucl_group = prop_group + t_crit_group * se_group
      )
  } else if (all(c("n_total", "prop") %in% names(.data))) {
    .data |>
      dplyr::mutate(
        se = sqrt(prop * (1 - prop) / (n_total - 1)),
        t_crit = stats::qt(t_prob, df = n_total - 1),
        lcl = prop - t_crit * se,
        ucl = prop + t_crit * se
      )
  } else {
    stop("wald_ci() could not find expected proportion/denominator columns.")
  }
}

#' Add logit-transformed confidence intervals to freq_tbl/freq_table output
#'
#' @inheritParams wald_ci
#' @return A tibble with lower/upper confidence limits.
#' @export
logit_ci <- function(.data, percent_ci = 95) {
  alpha <- 1 - (percent_ci / 100)
  t_prob <- 1 - alpha / 2

  if (all(c("n_group", "prop_group") %in% names(.data))) {
    .data |>
      dplyr::mutate(
        se_group = sqrt(prop_group * (1 - prop_group) / (n_group - 1)),
        t_crit_group = stats::qt(t_prob, df = n_group - 1),
        prop_group_log = log(prop_group) - log(1 - prop_group),
        se_group_log = se_group / (prop_group * (1 - prop_group)),
        lcl_group_log = prop_group_log - t_crit_group * se_group_log,
        ucl_group_log = prop_group_log + t_crit_group * se_group_log,
        lcl_group = exp(lcl_group_log) / (1 + exp(lcl_group_log)),
        ucl_group = exp(ucl_group_log) / (1 + exp(ucl_group_log))
      ) |>
      dplyr::select(-dplyr::any_of(c("prop_group_log", "se_group_log", "lcl_group_log", "ucl_group_log")))
  } else if (all(c("n_total", "prop") %in% names(.data))) {
    .data |>
      dplyr::mutate(
        se = sqrt(prop * (1 - prop) / (n_total - 1)),
        t_crit = stats::qt(t_prob, df = n_total - 1),
        prop_log = log(prop) - log(1 - prop),
        se_log = se / (prop * (1 - prop)),
        lcl_log = prop_log - t_crit * se_log,
        ucl_log = prop_log + t_crit * se_log,
        lcl = exp(lcl_log) / (1 + exp(lcl_log)),
        ucl = exp(ucl_log) / (1 + exp(ucl_log))
      ) |>
      dplyr::select(-dplyr::any_of(c("prop_log", "se_log", "lcl_log", "ucl_log")))
  } else {
    stop("logit_ci() could not find expected proportion/denominator columns.")
  }
}
