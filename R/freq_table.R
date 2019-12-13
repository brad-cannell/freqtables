#' @title Estimate Percents and 95 Percent Confidence Intervals in dplyr Pipelines
#'
#' @description The freq_table function produces one-way and two-way frequency
#'   tables for categorical variables. In addition to frequencies, the
#'   freq_table function displays percentages, and the standard errors and 95%
#'   confidence intervals of the percentages. For two-way tables only,
#'   freq_table also displays row (subgroup) percentages, standard errors,
#'   and 95 percent confidence intervals.
#'
#'   freq_table is intended to be used in a dplyr pipeline. Specifically,
#'   freq_table expects the x argument to be a grouped tibble created with
#'   dplyr's group_by function.
#'
#'   All standard errors are calculated as some version of:
#'   sqrt(proportion * (1 - proportion) / (n - 1))
#'
#'   For one-way tables, the default 95 percent confidence intervals displayed are
#'   logit transformed confidence intervals equivalent to those used by Stata.
#'   Additionally, freq_table will return Wald ("linear") confidence intervals
#'   if the argument to ci_type = "wald".
#'
#'   For two-way tables, freq_table returns logit transformed confidence
#'   intervals equivalent to those used by Stata.
#'
#' @param x A grouped tibble, i.e., class == "grouped_df".
#'
#'   For two-way tables, the count for each level of the variable in the
#'   first argument to group_by will be the denominator for row percentages
#'   and their 95% confidence intervals. Said another way, the goal of the
#'   analysis is to compare percentages of some characteristic across two or
#'   more groups of interest, then the variable in the first argument to
#'   group_by should contain the groups of interest, and the variable in the
#'   second argument to group_by should contain the characteristic of
#'   interest.
#'
#' @param t_prob (1 - alpha / 2). Default value is 0.975, which corresponds to
#'   an alpha of 0.05. Used to calculate a critical value from Student's t
#'   distribution with n - 1 degrees of freedom.
#'
#' @param ci_type Selects the method used to estimate 95 percent confidence intervals.
#'   The default for one-way and two-way tables is logit transformed ("log"). For
#'   one-way tables only, ci_type can optionally calculate Wald ("linear")
#'   confidence intervals using the "wald" argument.
#'
#' @param output Options for this parameter are "default" and "all".
#'
#'   For one-way tables with default output, the count, overall n, percent
#'   and 95 percent confidence interval are returned. Using output = "all" also
#'   returns the standard error of the percent and the critical t-value.
#'
#'   For two-way tables with default output, the count, group n, overall n, row
#'   percent, and 95 percent confidence interval for the row percent are
#'   returned. Using output = "all" also returns the overall percent, standard
#'   error of the percent, 95 percent confidence interval for the overall
#'   percent, the standard error of the row percent, and the critical t-values.
#'
#' @param digits Round percentages and confidence intervals to digits.
#'   Default is 2.
#'
#' @param ... Other parameters to be passed on.
#'
#' @return A tibble with class "freq_table_one_way" or "freq_table_two_way"
#' @export
#' @importFrom dplyr %>%
#'
#' @references
#'   Agresti, A. (2012). Categorical Data Analysis (3rd ed.). Hoboken, NJ: Wiley.
#'
#'   SAS documentation: https://support.sas.com/documentation/cdl/en/statug/63347/HTML/default/viewer.htm#statug_surveyfreq_a0000000221.htm
#'
#'   Stata documentation: https://www.stata.com/manuals13/rproportion.pdf
#'
#' @examples
#' library(dplyr)
#' library(freqtables)
#'
#' data(mtcars)
#'
#' # One-way frequency table with defaults
#'
#' mtcars %>%
#'   group_by(am) %>%
#'   freq_table()
#'
#' #> # A tibble: 2 x 7
#' #>     var   cat     n n_total percent   lcl   ucl
#' #>   <chr> <dbl> <int>   <int>   <dbl> <dbl> <dbl>
#' #> 1    am     0    19      32   59.38 40.94 75.50
#' #> 2    am     1    13      32   40.62 24.50 59.06
#'
#' # Two-way frequency table with defaults
#'
#' mtcars %>%
#'   group_by(am, cyl) %>%
#'   freq_table()
#'
#' #> # A tibble: 6 x 10
#' #>   row_var row_cat col_var col_cat     n n_row n_total percent_row lcl_row ucl_row
#' #>     <chr>   <dbl>   <chr>   <dbl> <int> <int>   <int>       <dbl>   <dbl>   <dbl>
#' #> 1      am       0     cyl       4     3    19      32       15.79    4.78   41.20
#' #> 2      am       0     cyl       6     4    19      32       21.05    7.58   46.44
#' #> 3      am       0     cyl       8    12    19      32       63.16   38.76   82.28
#' #> 4      am       1     cyl       4     8    13      32       61.54   32.30   84.29
#' #> 5      am       1     cyl       6     3    13      32       23.08    6.91   54.82
#' #> 6      am       1     cyl       8     2    13      32       15.38    3.43   48.18

freq_table <- function(x, t_prob = 0.975, ci_type = "logit", output = "default", digits = 2, ...) {

  # ------------------------------------------------------------------
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ------------------------------------------------------------------
  n = n_total = prop = t_crit = se = lcl_wald = ucl_wald = percent = NULL
  lcl = ucl = prop_log = se_log = lcl_log = ucl_log = prop_total = NULL
  se_total = prop_log_total = t_crit_total = se_log_total = lcl_total_log = NULL
  percent_total = n_row = prop_row = se_row = prop_log_row = t_crit_row = NULL
  se_log_row = lcl_row_log = ucl_row_log = percent_row = lcl_row = NULL
  ucl_row = lcl_total = ucl_total = ucl_total_log = n_groups = NULL
  ci_type_arg = output_arg = `.` = var = row_var = row_cat = NULL
  col_var = col_cat = NULL

  # ===========================================================================
  # Check for grouped tibble
  # ===========================================================================
  if (!("grouped_df" %in% class(x))) {
    stop(paste("The x argument to freq_table must be a grouped tibble.
               The class of the current x argument is", class(x)))
  }

  # ===========================================================================
  # Enquo arguments
  # enquo/quo_name/UQ the ci_type and output argument so that I don't have to
  # use quotation marks around the argument being passed.
  # ===========================================================================
  ci_type_arg <- rlang::enquo(ci_type) %>% rlang::quo_name()
  output_arg  <- rlang::enquo(output) %>% rlang::quo_name()

  # ===========================================================================
  # Check for number of group vars:
  # ===========================================================================
  n_groups <- attributes(x)$groups %>% length() - 1

  # ===========================================================================
  # One-way tables
  # ===========================================================================
  if (n_groups == 1) { # "else" is in two-way tables.

    # Create first three columns of summary table: grouped variable name,
    # grouped variable categories, and n of each category
    out <- x %>%
      dplyr::summarise(n = n()) %>%
      dplyr::mutate(var = !!names(.[1])) %>%
      dplyr::rename(cat = !!names(.[1])) %>%
      dplyr::select(var, cat, n)

    # Update out to include elements needed for Wald and Logit transformed CI's
    # One-way tables
    out <- out %>%
      dplyr::mutate(
        n_total = sum(n),
        prop    = n / n_total,
        se      = sqrt(prop * (1 - prop) / (n_total - 1)),
        t_crit  = stats::qt(t_prob, df = n_total - 1)
      )

    # Calculate Wald CI's
    # -------------------
    # and put prop, se, and CI's on percent scale
    # One-way tables
    if (ci_type_arg == "wald") {

      out <- out %>%
        dplyr::mutate(
          lcl_wald = prop - t_crit * se,
          ucl_wald = prop + t_crit * se,
          percent  = prop * 100,
          se       = se * 100,
          lcl_wald = lcl_wald * 100,
          ucl_wald = ucl_wald * 100,
          percent  = round(percent, digits),  # Round percent
          lcl      = round(lcl_wald, digits), # Round confidence intervals
          ucl      = round(ucl_wald, digits)
        )

      # Control output
      # Typically, I only want the frequency, percent and 95% CI
      # Make that the default
      if (output_arg == "default") {
        out <- out %>%
          dplyr::select(var, cat, n, n_total, percent, lcl, ucl)

      } else if (output_arg == "all") {
        out <- out %>%
          dplyr::select(var, cat, n, n_total, percent, se, t_crit, lcl, ucl)
      }

      # Calculate logit transformed CI's
      # ------------------------------
      # and put prop, se, and CI's on percent scale
      # One-way tables
    } else if (ci_type_arg == "logit") {

      out <- out %>%
        dplyr::mutate(
          prop_log = log(prop) - log(1 - prop),
          se_log   = se / (prop * (1 - prop)),
          lcl_log  = prop_log - t_crit * se_log,
          ucl_log  = prop_log + t_crit * se_log,
          lcl_log  = exp(lcl_log) / (1 + exp(lcl_log)),
          ucl_log  = exp(ucl_log) / (1 + exp(ucl_log)),
          percent  = prop * 100,
          se       = se * 100,
          lcl_log  = lcl_log * 100,
          ucl_log  = ucl_log * 100,
          percent  = round(percent, digits), # Round percent
          lcl      = round(lcl_log, digits), # Round confidence intervals
          ucl      = round(ucl_log, digits)
        )

      # Control output
      # Typically, I only want the frequency, percent and 95% CI
      # Make that the default
      if (output_arg == "default") {
        out <- out %>%
          dplyr::select(var, cat, n, n_total, percent, lcl, ucl)

      } else if (output_arg == "all") {
        out <- out %>%
          dplyr::select(var, cat, n, n_total, percent, se, t_crit, lcl, ucl)
      }

    }

    # Add freq_table class to out
    class(out) <- c("freq_table_one_way", class(out))


    # ===========================================================================
    # Two-way tables
    # Only logit transformed CI's
    # Need percent and row percent
    # ===========================================================================
  } else if (n_groups == 2) { # "if" is one-way tables

    # Create first three columns of summary table: row variable name,
    # row variable categories, column variable name, column variable categories
    # and n of each category row/col combination
    out <- x %>%
      dplyr::summarise(n = n()) %>%
      dplyr::mutate(
        row_var = !!names(.[1]),
        col_var = !!names(.[2])
      ) %>%
      dplyr::rename(
        row_cat = !!names(.[1]),
        col_cat = !!names(.[2])
      ) %>%
      dplyr::select(row_var, row_cat, col_var, col_cat, n) %>%

      # Calculate within row n
      dplyr::mutate(n_row = sum(n)) %>%
      # Ungroup to get total_n
      dplyr::ungroup() %>%
      dplyr::mutate(

        # Estimate overall percent se and CI's
        n_total        = sum(n),
        prop_total     = n / n_total,
        se_total       = sqrt(prop_total * (1 - prop_total) / (n_total - 1)),
        t_crit_total   = stats::qt(t_prob, df = n_total - 1),
        prop_log_total = log(prop_total) - log(1 - prop_total),
        se_log_total   = se_total / (prop_total * (1 - prop_total)),
        lcl_total_log  = prop_log_total - t_crit_total * se_log_total,
        ucl_total_log  = prop_log_total + t_crit_total * se_log_total,
        lcl_total_log  = exp(lcl_total_log) / (1 + exp(lcl_total_log)),
        ucl_total_log  = exp(ucl_total_log) / (1 + exp(ucl_total_log)),
        percent_total  = prop_total * 100,
        se_total       = se_total * 100,
        lcl_total_log  = lcl_total_log * 100,
        ucl_total_log  = ucl_total_log * 100,
        percent_total  = round(percent_total, digits), # Round percent
        lcl_total      = round(lcl_total_log, digits), # Round confidence intervals
        ucl_total      = round(ucl_total_log, digits),


        # Estimate row percent se and CI's
        prop_row     = n / n_row,
        se_row       = sqrt(prop_row * (1 - prop_row) / (n_row - 1)), # group n - 1
        t_crit_row   = stats::qt(t_prob, df = n_total - 1), # overall n - 1
        prop_log_row = log(prop_row) - log(1 - prop_row),
        se_log_row   = se_row / (prop_row * (1 - prop_row)),
        lcl_row_log  = prop_log_row - t_crit_row * se_log_row,
        ucl_row_log  = prop_log_row + t_crit_row * se_log_row,
        lcl_row_log  = exp(lcl_row_log) / (1 + exp(lcl_row_log)),
        ucl_row_log  = exp(ucl_row_log) / (1 + exp(ucl_row_log)),
        percent_row  = prop_row * 100,
        se_row       = se_row * 100,
        lcl_row_log  = lcl_row_log * 100,
        ucl_row_log  = ucl_row_log * 100,
        percent_row  = round(percent_row, digits), # Round percent
        lcl_row      = round(lcl_row_log, digits), # Round confidence intervals
        ucl_row      = round(ucl_row_log, digits)
      )

    # Control output
    # Typically, I only want the frequency, row percent and 95% CI for the row percent
    # Make that the default
    if (output_arg == "default") {
      out <- out %>%
        dplyr::select(row_var, row_cat, col_var, col_cat, n, n_row, n_total,
                      percent_row, lcl_row, ucl_row)

    } else if (output_arg == "all") {
      out <- out %>%
        dplyr::select(row_var, row_cat, col_var, col_cat, n, n_row, n_total,
                      percent_total, se_total, t_crit_total,
                      lcl_total, ucl_total, percent_row, se_row, t_crit_row,
                      lcl_row, ucl_row)
    }

    # Add freq_table class to out
    class(out) <- c("freq_table_two_way", class(out))

  } else { # Grouped by more than two variables, or not grouped.
    stop(
      paste(
        "Expecting x to be a grouped data frame with 2 or 3 columns. Instead
        x had", ncol(out)
      )
    )
  }

  # Return tibble of results
  out
}
