#' @title Estimate Percents and 95 Percent Confidence Intervals in dplyr Pipelines
#'
#' @description The freq_table function produces one-way and two-way frequency
#'   tables for categorical variables. In addition to frequencies, the
#'   freq_table function displays percentages, and the standard errors and 95%
#'   confidence intervals of the percentages. For two-way tables only,
#'   freq_table also displays row (subgroup) percentages, standard errors,
#'   and 95 percent confidence intervals.
#'
#'   freq_table is intended to be used in a dplyr pipeline.
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
#' @param .data A data frame. If it is already grouped (i.e., class == "grouped_df")
#'   then freq_table will ungroup it to prevent unexpected results.
#'
#'   For two-way tables, the count for each level of the variable in the
#'   first argument to freq_table will be the denominator for row percentages
#'   and their 95% confidence intervals. Said another way, the goal of the
#'   analysis is to compare percentages of some characteristic across two or
#'   more groups of interest, then the variable in the first argument to
#'   freq_table should contain the groups of interest, and the variable in the
#'   second argument to freq_table should contain the characteristic of
#'   interest.
#'
#' @param ... Categorical variables to be used in calculations. Currently,
#'   freq_table accepts one or two variables -- not more.
#'
#'   If "..." includes a factor variable with a level (category) that is
#'   unobserved in the data, that level will still appear in the results with
#'   a count (n) equal to zero. Further, the confidence intervals will be
#'   equal to NaN.
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
#'   freq_table(am)
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
#'   freq_table(am, cyl)
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
freq_table <- function(.data, ..., t_prob = 0.975, ci_type = "logit") {

  # ------------------------------------------------------------------
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ------------------------------------------------------------------
  n = n_total = prop = t_crit = se = lcl_wald = ucl_wald = percent = NULL
  lcl = ucl = prop_log = se_log = lcl_log = ucl_log = prop_total = NULL
  se_total = prop_log_total = t_crit_total = se_log_total = lcl_total_log = NULL
  percent_total = n_row = prop_row = se_row = prop_log_row = t_crit_row = NULL
  se_log_row = lcl_row_log = ucl_row_log = percent_row = lcl_row = NULL
  ucl_row = lcl_total = ucl_total = ucl_total_log = n_groups = NULL
  ci_type_arg = var = row_var = row_cat = NULL
  col_var = col_cat = `.` = NULL

  # ===========================================================================
  # Enquo arguments
  # enquo/quo_name/UQ the ci_type and output argument so that I don't have to
  # use quotation marks around the argument being passed.
  # ===========================================================================
  ci_type_arg <- rlang::enquo(ci_type) %>% rlang::quo_name()

  # ===========================================================================
  # Check for grouped tibble
  # Check to see if the tibble is already grouped.
  # If yes, ungroup, so that you don't get unexpected results.
  # Then, group here using the variables in ...
  # ===========================================================================
  if (("grouped_df" %in% class(.data))) {
    .data <- dplyr::ungroup(.data)
  }

  # ===========================================================================
  # Get within group counts
  # .drop = FALSE creates an explicit n = 0 for unobserved factor levels
  # ===========================================================================
  .data <- dplyr::count(.data, ..., .drop = FALSE)

  # ===========================================================================
  # Check for number of group vars:
  # ===========================================================================
  n_groups <- .data %>% ncol() - 1
  if (n_groups > 2) {
    stop("Currently, freq_table accepts one or two variables -- not more. You entered ",
         n_groups, " into the ... argument.")
  }

  # ===========================================================================
  # One-way tables
  # ===========================================================================
  if (n_groups == 1) {

    # Create first three columns of summary table: grouped variable name,
    # grouped variable categories, and n of each category
    out <- .data %>%
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
          lcl      = lcl_wald * 100,
          ucl      = ucl_wald * 100
        )

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
          lcl      = lcl_log * 100,
          ucl      = ucl_log * 100
        )
    }

    # Control output
    out <- out %>%
      dplyr::select(var, cat, n, n_total, percent, se, t_crit, lcl, ucl)

    # Add freq_table class to out
    class(out) <- c("freq_table_one_way", class(out))
  }

  # ===========================================================================
  # Two-way tables
  # Only logit transformed CI's
  # Need percent and row percent
  # ===========================================================================
  if (n_groups == 2) {

    # Create first three columns of summary table: row variable name,
    # row variable categories, column variable name, column variable categories
    # and n of each category row/col combination
    out <- .data %>%
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
      dplyr::group_by(row_cat) %>%
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
        lcl_total      = lcl_total_log * 100,
        ucl_total      = ucl_total_log * 100,


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
        lcl_row      = lcl_row_log * 100,
        ucl_row      = ucl_row_log * 100
      )

    # Control output
    # Typically, I only want the frequency, row percent and 95% CI for the row percent
    # Make that the default
    out <- out %>%
      dplyr::select(row_var, row_cat, col_var, col_cat, n, n_row, n_total,
                    percent_total, se_total, t_crit_total,
                    lcl_total, ucl_total, percent_row, se_row, t_crit_row,
                    lcl_row, ucl_row)

    # Add freq_table class to out
    class(out) <- c("freq_table_two_way", class(out))
  }

  # Return tibble of results
  out
}
