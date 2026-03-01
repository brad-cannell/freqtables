#' @title Estimate Counts, Proportions, and Confidence Intervals in dplyr
#'   Pipelines
#'
#' @description The freq_table function produces frequency tables for a single
#'   categorical variable with counts, proportions, and confidence intervals.
#'   It is designed to work with \code{dplyr::group_by()} for grouped analyses.
#'
#'   By default, freq_table returns logit-transformed confidence intervals
#'   equivalent to those used by Stata. Wald ("linear") confidence intervals
#'   can be requested via the \code{ci_type} parameter.
#'
#'   All standard errors are calculated as:
#'   \code{sqrt(proportion * (1 - proportion) / (n - 1))}
#'
#' @param .data A data frame or grouped data frame. Use
#'   \code{dplyr::group_by()} for grouped analyses.
#'
#' @param ... A single categorical variable to tabulate. Passing more than one
#'   variable is deprecated; use \code{dplyr::group_by()} instead.
#'
#' @param percent_ci The confidence level as a percentage. Default is 95 for
#'   95\% confidence intervals.
#'
#' @param ci_type Method for confidence interval estimation. Either "logit"
#'   (default, matches Stata) or "wald" (matches SAS).
#'
#' @param drop If FALSE (default), unobserved factor levels will appear in the
#'   results with n = 0.
#'
#' @param generic_col_names If FALSE (default), variable names are used as
#'   column headers. If TRUE, generic column headers are used (e.g.,
#'   group_01_col, group_01_cat, col, cat) for easier programming.
#'
#' @param overall If FALSE (default), overall proportions and confidence
#'   intervals are not included. If TRUE, they are added.
#'
#' @param se If FALSE (default), standard errors are not included in the
#'   output. If TRUE, they are added.
#'
#' @param critical_value If FALSE (default), critical values are not included
#'   in the output. If TRUE, they are added.
#'
#' @param percent If FALSE (default), results are returned as proportions. If
#'   TRUE, results are returned as percentages.
#'
#' @return A tibble with class "freq_table_one_way" or "freq_table_two_way".
#' @export
#' @importFrom dplyr %>%
#'
#' @references
#'  Agresti, A. (2012). Categorical Data Analysis (3rd ed.). Hoboken, NJ:
#'  Wiley.
#'
#' @examples
#' library(dplyr)
#' library(freqtables)
#'
#' data(mtcars)
#'
#' # One-way frequency table
#' mtcars %>%
#'   freq_table(am)
#'
#' # Grouped frequency table using group_by
#' mtcars %>%
#'   group_by(cyl) %>%
#'   freq_table(am)
#'
#' # With Wald confidence intervals
#' mtcars %>%
#'   freq_table(am, ci_type = "wald")
#'
#' # With generic column names for programming
#' mtcars %>%
#'   group_by(cyl) %>%
#'   freq_table(am, generic_col_names = TRUE)
freq_table <- function(.data, ..., percent_ci = 95, ci_type = "logit",
                       drop = FALSE, generic_col_names = FALSE,
                       overall = FALSE, se = FALSE, critical_value = FALSE,
                       percent = FALSE) {

  # ------------------------------------------------------------------
  # Prevents R CMD check: "no visible binding for global variable '.'"
  # ------------------------------------------------------------------
  n <- n_total <- prop_val <- t_crit_val <- se_val <- NULL
  lcl_wald <- ucl_wald <- prop_log <- se_log <- lcl_log <- ucl_log <- NULL
  n_group <- prop_group <- se_group <- t_crit_group <- NULL
  prop_log_group <- se_log_group <- lcl_log_group <- ucl_log_group <- NULL
  lcl_group <- ucl_group <- NULL
  prop_overall <- se_overall <- t_crit_overall <- NULL
  prop_log_overall <- se_log_overall <- lcl_log_overall <- ucl_log_overall <- NULL
  lcl_overall <- ucl_overall <- NULL

  # ===========================================================================
  # Enquo the ci_type argument
  # ===========================================================================
  ci_type_arg <- rlang::enquo(ci_type) %>% rlang::quo_name()

  # ===========================================================================
  # Check that .data is a data frame
  # ===========================================================================
  if (!is.data.frame(.data)) {
    stop(
      "freq_table expects a data frame to be passed to the .data argument. ",
      "The object passed has class: ",
      paste(class(.data), collapse = ", "),
      ". Please use the form: mtcars %>% freq_table(am)"
    )
  }

  # ===========================================================================
  # Capture the column(s) passed via ...
  # ===========================================================================
  col_quos <- rlang::enquos(...)

  # ===========================================================================
  # Validate number of columns
  # ===========================================================================
  if (length(col_quos) == 0) {
    stop(
      "No column names were passed to freq_table(). ",
      "Please pass a column name. For example: mtcars %>% freq_table(am)"
    )
  }

  if (length(col_quos) > 1) {
    # Build a helpful deprecation message
    col_names <- vapply(col_quos, rlang::as_name, character(1))
    group_cols <- paste(col_names[-length(col_names)], collapse = ", ")
    analysis_col <- col_names[length(col_names)]
    stop(
      "Passing more than one column name to the freq_table function is ",
      "deprecated. Please use the dplyr::group_by() function to perform ",
      "grouped analyses. For example, use `mtcars |> group_by(",
      group_cols, ") |> freq_table(", analysis_col,
      ")` instead of `mtcars |> freq_table(",
      paste(col_names, collapse = ", "), ")`."
    )
  }

  # ===========================================================================
  # Get the single analysis column
  # ===========================================================================
  col_quo <- col_quos[[1]]
  col_name <- rlang::as_name(col_quo)

  # ===========================================================================
  # Detect grouping variables
  # ===========================================================================
  group_vars <- dplyr::group_vars(.data)
  is_grouped <- length(group_vars) > 0

  # ===========================================================================
  # Calculate alpha and t_prob
  # ===========================================================================
  alpha <- 1 - (percent_ci / 100)
  t_prob <- 1 - alpha / 2

  # ===========================================================================
  # One-way tables (no grouping)
  # ===========================================================================
  if (!is_grouped) {

    # Get counts
    out <- dplyr::count(.data, !!col_quo, .drop = drop)
    n_total_val <- sum(out$n)

    # Calculate proportion and CI
    out <- out %>%
      dplyr::mutate(
        prop_val    = n / sum(n),
        se_val      = sqrt(prop_val * (1 - prop_val) / (n_total_val - 1)),
        t_crit_val  = stats::qt(t_prob, df = n_total_val - 1)
      )

    # Calculate CI based on ci_type
    if (ci_type_arg == "wald") {
      out <- out %>%
        dplyr::mutate(
          lcl_log = prop_val - t_crit_val * se_val,
          ucl_log = prop_val + t_crit_val * se_val
        )
    } else {
      # logit (default)
      out <- out %>%
        dplyr::mutate(
          prop_log = log(prop_val) - log(1 - prop_val),
          se_log   = se_val / (prop_val * (1 - prop_val)),
          lcl_log  = prop_log - t_crit_val * se_log,
          ucl_log  = prop_log + t_crit_val * se_log,
          lcl_log  = exp(lcl_log) / (1 + exp(lcl_log)),
          ucl_log  = exp(ucl_log) / (1 + exp(ucl_log))
        )
    }

    # Build output columns
    if (percent) {
      out$prop_val <- out$prop_val * 100
      out$se_val <- out$se_val * 100
      out$lcl_log <- out$lcl_log * 100
      out$ucl_log <- out$ucl_log * 100
    }

    # Select output columns based on parameters
    if (generic_col_names) {
      out_tbl <- dplyr::tibble(
        col = col_name,
        cat = as.character(out[[col_name]]),
        n = out$n,
        prop = out$prop_val,
        lcl = out$lcl_log,
        ucl = out$ucl_log
      )

      if (se) out_tbl$se <- out$se_val
      if (critical_value) out_tbl$t_crit <- out$t_crit_val

    } else {
      out_tbl <- dplyr::tibble(
        !!col_name := out[[col_name]],
        n = out$n,
        prop = out$prop_val,
        lcl = out$lcl_log,
        ucl = out$ucl_log
      )

      if (se) out_tbl$se <- out$se_val
      if (critical_value) out_tbl$t_crit <- out$t_crit_val
    }

    if (percent) {
      # Rename prop to percent in the output
      names(out_tbl)[names(out_tbl) == "prop"] <- "percent"
    }

    class(out_tbl) <- c("freq_table_one_way", class(out_tbl))
    attr(out_tbl, "col_var") <- col_name
    attr(out_tbl, "group_vars") <- character(0)
    out <- out_tbl

  } else {
    # ===========================================================================
    # Grouped tables
    # ===========================================================================

    # Get counts within groups
    out <- dplyr::count(.data, !!col_quo, .drop = drop)

    # Calculate group-level statistics
    out <- out %>%
      dplyr::mutate(
        n_group     = sum(n),
        prop_group  = n / n_group,
        se_group    = sqrt(prop_group * (1 - prop_group) / (n_group - 1)),
        t_crit_group = stats::qt(t_prob, df = n_group - 1)
      )

    # Calculate group CI based on ci_type
    if (ci_type_arg == "wald") {
      out <- out %>%
        dplyr::mutate(
          lcl_group = prop_group - t_crit_group * se_group,
          ucl_group = prop_group + t_crit_group * se_group
        )
    } else {
      # logit (default)
      out <- out %>%
        dplyr::mutate(
          prop_log_group = log(prop_group) - log(1 - prop_group),
          se_log_group   = se_group / (prop_group * (1 - prop_group)),
          lcl_log_group  = prop_log_group - t_crit_group * se_log_group,
          ucl_log_group  = prop_log_group + t_crit_group * se_log_group,
          lcl_group      = exp(lcl_log_group) / (1 + exp(lcl_log_group)),
          ucl_group      = exp(ucl_log_group) / (1 + exp(ucl_log_group))
        )
    }

    # Calculate overall statistics if requested
    if (overall) {
      out <- out %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          n_total        = sum(n),
          prop_overall   = n / n_total,
          se_overall     = sqrt(prop_overall * (1 - prop_overall) / (n_total - 1)),
          t_crit_overall = stats::qt(t_prob, df = n_total - 1)
        )

      if (ci_type_arg == "wald") {
        out <- out %>%
          dplyr::mutate(
            lcl_overall = prop_overall - t_crit_overall * se_overall,
            ucl_overall = prop_overall + t_crit_overall * se_overall
          )
      } else {
        out <- out %>%
          dplyr::mutate(
            prop_log_overall = log(prop_overall) - log(1 - prop_overall),
            se_log_overall   = se_overall / (prop_overall * (1 - prop_overall)),
            lcl_log_overall  = prop_log_overall - t_crit_overall * se_log_overall,
            ucl_log_overall  = prop_log_overall + t_crit_overall * se_log_overall,
            lcl_overall      = exp(lcl_log_overall) / (1 + exp(lcl_log_overall)),
            ucl_overall      = exp(ucl_log_overall) / (1 + exp(ucl_log_overall))
          )
      }
    }

    # Ungroup for output construction
    out <- dplyr::ungroup(out)

    # Apply percent scaling if requested
    if (percent) {
      out$prop_group <- out$prop_group * 100
      out$se_group <- out$se_group * 100
      out$lcl_group <- out$lcl_group * 100
      out$ucl_group <- out$ucl_group * 100
      if (overall) {
        out$prop_overall <- out$prop_overall * 100
        out$se_overall <- out$se_overall * 100
        out$lcl_overall <- out$lcl_overall * 100
        out$ucl_overall <- out$ucl_overall * 100
      }
    }

    # Build output tibble based on column naming preference
    if (generic_col_names) {
      # Generic column names for programming
      out_tbl <- dplyr::tibble()

      # Add group columns
      for (i in seq_along(group_vars)) {
        gv <- group_vars[i]
        idx <- sprintf("%02d", i)
        out_tbl[[paste0("group_", idx, "_col")]] <- gv
        out_tbl[[paste0("group_", idx, "_cat")]] <- as.character(out[[gv]])
      }

      # Add analysis column
      out_tbl$col <- col_name
      out_tbl$cat <- as.character(out[[col_name]])
      out_tbl$n <- out$n
      out_tbl$n_group <- out$n_group

      if (percent) {
        out_tbl$percent_group <- out$prop_group
      } else {
        out_tbl$prop_group <- out$prop_group
      }

      if (se) out_tbl$se_group <- out$se_group
      if (critical_value) out_tbl$t_crit_group <- out$t_crit_group

      out_tbl$lcl_group <- out$lcl_group
      out_tbl$ucl_group <- out$ucl_group

      if (overall) {
        out_tbl$n_total <- out$n_total
        if (percent) {
          out_tbl$percent_overall <- out$prop_overall
        } else {
          out_tbl$prop_overall <- out$prop_overall
        }
        if (se) out_tbl$se_overall <- out$se_overall
        if (critical_value) out_tbl$t_crit_overall <- out$t_crit_overall
        out_tbl$lcl_overall <- out$lcl_overall
        out_tbl$ucl_overall <- out$ucl_overall
      }

    } else {
      # Variable names as column headers (default)
      out_tbl <- dplyr::tibble()

      # Add group columns with their actual names
      for (gv in group_vars) {
        out_tbl[[gv]] <- out[[gv]]
      }

      # Add analysis column with its actual name
      out_tbl[[col_name]] <- out[[col_name]]
      out_tbl$n <- out$n
      out_tbl$n_group <- out$n_group

      if (percent) {
        out_tbl$percent_group <- out$prop_group
      } else {
        out_tbl$prop_group <- out$prop_group
      }

      if (se) out_tbl$se_group <- out$se_group
      if (critical_value) out_tbl$t_crit_group <- out$t_crit_group

      out_tbl$lcl_group <- out$lcl_group
      out_tbl$ucl_group <- out$ucl_group

      if (overall) {
        out_tbl$n_total <- out$n_total
        if (percent) {
          out_tbl$percent_overall <- out$prop_overall
        } else {
          out_tbl$prop_overall <- out$prop_overall
        }
        if (se) out_tbl$se_overall <- out$se_overall
        if (critical_value) out_tbl$t_crit_overall <- out$t_crit_overall
        out_tbl$lcl_overall <- out$lcl_overall
        out_tbl$ucl_overall <- out$ucl_overall
      }
    }

    class(out_tbl) <- c("freq_table_two_way", class(out_tbl))
    attr(out_tbl, "col_var") <- col_name
    attr(out_tbl, "group_vars") <- group_vars
    out <- out_tbl
  }

  out
}
