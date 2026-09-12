#' Add confidence intervals to frequency counts
#'
#' These helpers add intervals to [freq_tbl()] output, and are also used by
#' [freq_table()]. Both retain the package's Student-t convention:
#' `SE = sqrt(p * (1 - p) / (N - 1))`, with `N - 1` degrees of freedom.
#' For within-group intervals, N is that group's denominator; for overall
#' intervals, N is the total input size. This is a compatibility convention,
#' not the usual normal-z binomial Wald interval.
#'
#' `wald_ci()` returns `p +/- t * SE` without clipping to the unit interval.
#' `logit_ci()` transforms `log(p / (1 - p)) +/- t * SE / (p * (1 - p))`
#' back to the proportion scale with [stats::plogis()]. At p = 0 or 1,
#' logit bounds are missing (no continuity correction); Wald bounds equal p.
#' With N <= 1, SE, critical values, and both types of bounds are missing.
#'
#' @param .data A frequency table from [freq_tbl()] or [freq_table()].
#'   A plain data frame is also accepted if it contains `n` and
#'   `n_group` with `prop_group`/`percent_group`, or `n_total` with
#'   `prop`/`percent`. Optional overall columns use
#'   `n_total` with `prop_total`/`percent_total`.
#'   An ungrouped compact table needs its original freqtables metadata or an
#'   explicit `n_total`; totals are never reconstructed by summing a possibly
#'   filtered table. Counts must be finite nonnegative integers no larger than
#'   their denominators. Displayed proportions must agree with those counts.
#' @param percent_ci Confidence level as a percentage, strictly between 0 and
#'   100; default 95. Independent of the table's proportion/percentage scale.
#' @param se Logical. Include standard errors? Default `FALSE`.
#' @param critical_value Logical. Include Student-t critical values in
#'   `t_crit`, `t_crit_group`, and/or `t_crit_total`? Default `FALSE`.
#' @return The input table with `lcl`/`ucl`, `lcl_group`/`ucl_group`,
#'   and/or `lcl_total`/`ucl_total` for each proportion present.
#'   Bounds and standard errors use the input scale; critical values do not
#'   scale. Repeated calls replace interval/detail columns for the same
#'   proportions, including removal of unrequested optional detail columns.
#' @name confidence_intervals
#' @export
#' @examples
#' freq_tbl(mtcars, am) |> wald_ci()
#' freq_tbl(mtcars, am) |> logit_ci(percent_ci = 99, se = TRUE)
#' mtcars |> dplyr::group_by(cyl) |>
#'   freq_tbl(am, percent = TRUE, overall = TRUE) |>
#'   logit_ci(critical_value = TRUE)
wald_ci <- function(.data, percent_ci = 95, se = FALSE,
                    critical_value = FALSE) {
  add_frequency_ci(.data, percent_ci, se, critical_value, "wald")
}

#' @rdname confidence_intervals
#' @export
logit_ci <- function(.data, percent_ci = 95, se = FALSE,
                     critical_value = FALSE) {
  add_frequency_ci(.data, percent_ci, se, critical_value, "logit")
}

add_frequency_ci <- function(data, level, include_se, include_critical, method) {
  check_flag(include_se, "se")
  check_flag(include_critical, "critical_value")
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 100) {
    stop("percent_ci must be a number strictly between 0 and 100.", call. = FALSE)
  }
  if (!is.data.frame(data) || !is.numeric(data$n)) {
    stop(".data must contain numeric counts in n.", call. = FALSE)
  }
  counts <- data$n
  if (any(!is.finite(counts) | counts < 0 | counts != floor(counts))) {
    stop("n must contain finite nonnegative integer counts.", call. = FALSE)
  }
  metadata <- attr(data, "freqtables", exact = TRUE)
  found <- FALSE
  for (suffix in c("", "_group", "_total")) {
    columns <- intersect(paste0(c("prop", "percent"), suffix), names(data))
    if (!length(columns)) next
    if (length(columns) != 1L) {
      stop("Supply either proportions or percentages, not both for a statistic.",
           call. = FALSE)
    }
    found <- TRUE
    column <- columns[1]
    denominator_column <- if (suffix == "_group") "n_group" else "n_total"
    denominator <- data[[denominator_column]]
    if (is.null(denominator) && denominator_column == "n_total" &&
        !is.null(metadata$n_total)) {
      denominator <- rep(metadata$n_total, nrow(data))
    }
    if (is.null(denominator)) {
      stop("Missing ", denominator_column,
           "; retain freqtables metadata or supply the original denominator.",
           call. = FALSE)
    }
    if (!is.numeric(denominator) || length(denominator) != nrow(data) ||
        any(!is.finite(denominator) | denominator < counts |
            denominator < 0 | denominator != floor(denominator))) {
      stop(denominator_column,
           " must contain finite integer denominators at least as large as n.",
           call. = FALSE)
    }
    p <- safe_proportion(counts, denominator)
    scale <- if (startsWith(column, "percent")) 100 else 1
    displayed <- data[[column]]
    if (!is.numeric(displayed) ||
        any(is.na(displayed) != is.na(p)) ||
        any(abs(displayed / scale - p) > 1e-8, na.rm = TRUE) ||
        any(is.infinite(displayed))) {
      stop(column, " does not agree with n and its denominator.", call. = FALSE)
    }
    valid <- denominator > 1
    standard_error <- critical <- lower <- upper <- rep(NA_real_, nrow(data))
    standard_error[valid] <- sqrt(p[valid] * (1 - p[valid]) /
                                   (denominator[valid] - 1))
    critical[valid] <- stats::qt((1 + level / 100) / 2,
                                 df = denominator[valid] - 1)
    if (method == "wald") {
      lower[valid] <- p[valid] - critical[valid] * standard_error[valid]
      upper[valid] <- p[valid] + critical[valid] * standard_error[valid]
    } else {
      interior <- valid & !is.na(p) & p > 0 & p < 1
      half_width <- critical[interior] * standard_error[interior] /
        (p[interior] * (1 - p[interior]))
      lower[interior] <- stats::plogis(stats::qlogis(p[interior]) - half_width)
      upper[interior] <- stats::plogis(stats::qlogis(p[interior]) + half_width)
    }
    # Clear stale details on repeated calls before appending requested columns.
    for (stem in c("lcl", "ucl", "se", "t_crit")) data[[paste0(stem, suffix)]] <- NULL
    if (include_se) data[[paste0("se", suffix)]] <- standard_error * scale
    if (include_critical) data[[paste0("t_crit", suffix)]] <- critical
    data[[paste0("lcl", suffix)]] <- lower * scale
    data[[paste0("ucl", suffix)]] <- upper * scale
  }
  if (!found) {
    stop("No supported proportion or percentage columns found.", call. = FALSE)
  }
  data
}
