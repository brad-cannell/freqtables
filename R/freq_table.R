#' @title Estimate Counts, Percentages, and Confidence Intervals in dplyr Pipelines
#'
#' @description The freq_table function produces one-way and two-way frequency
#'   tables for categorical variables. In addition to frequencies, the
#'   freq_table function displays percentages, and the standard errors and
#'   confidence intervals of the percentages. For two-way tables only,
#'   freq_table also displays row (subgroup) percentages, standard errors,
#'   and confidence intervals.
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
#'   and their confidence intervals. Said another way, the goal of the
#'   analysis is to compare percentages of some characteristic across two or
#'   more groups of interest, then the variable in the first argument to
#'   freq_table should contain the groups of interest, and the variable in the
#'   second argument to freq_table should contain the characteristic of
#'   interest.
#'
#' @param ... Categorical variables to be used in calculations. Currently,
#'   freq_table accepts one or two variables -- not more.
#'
#'   By default, if ... includes a factor variable with a level (category)
#'   that is unobserved in the data, that level will still appear in the
#'   results with a count (n) equal to zero. This behavior can be changed using
#'   the drop parameter (see below). When n = 0, the confidence intervals
#'   will be NaN.
#'
#' @param percent_ci sets the level, as a percentage, for confidence intervals.
#'   The default is percent_ci = 95 for 95% confidence intervals. The
#'   percentage value entered (e.g., 95) is converted to an alpha level as
#'   1 - (percent_ci / 100). It is then converted to a two-sided probability
#'   as (1 - alpha / 2), which is used to calculate a critical value from
#'   Student's t distribution with n - 1 degrees of freedom.
#'
#' @param ci_type Selects the method used to estimate 95 percent confidence intervals.
#'   The default for one-way and two-way tables is logit transformed ("log"). For
#'   one-way tables only, ci_type can optionally calculate Wald ("linear")
#'   confidence intervals using the "wald" argument.
#'
#' @param drop If false (default) unobserved factor levels will be included in
#'   the returned frequency table with an n of 0. For example, if you have a
#'   factor variable, gender, but no males in your data then frequency table
#'   returned by freq_table(df, gender) would still contain a row for
#'   males with the variable n = 0. If drop is set to TRUE, then the resulting
#'   frequency table would not include a row for males at all.
#'
#' @return A tibble with class "freq_table_one_way" or "freq_table_two_way"
#' @export
#' @importFrom dplyr %>%
#'
#' @references
#'  Agresti, A. (2012). Categorical Data Analysis (3rd ed.). Hoboken, NJ: Wiley.
#'
#'  \href{https://support.sas.com/documentation/cdl/en/statug/63347/HTML/default/viewer.htm#statug_surveyfreq_a0000000221.htm}{SAS confidence limits for proportions documentation}
#'
#'  \href{https://www.stata.com/manuals13/rproportion.pdf}{Stata confidence limits for proportions documentation}
#'
#' @examples
#' library(dplyr)
#' library(freqtables)
#'
#' data(mtcars)
#'
#' # --------------------------------------------------------------------------
#' # One-way frequency table with defaults
#' #   - The default confidence intervals are logit transformed - matching the
#' #     method used by Stata
#' # --------------------------------------------------------------------------
#' mtcars %>%
#'   freq_table(am)
#'
#' #   A tibble: 2 x 9
#' #   var   cat       n n_total percent    se t_crit   lcl   ucl
#' #   <chr> <chr> <int>   <int>   <dbl> <dbl>  <dbl> <dbl> <dbl>
#' # 1 am    0        19      32    59.4  8.82   2.04  40.9  75.5
#' # 2 am    1        13      32    40.6  8.82   2.04  24.5  59.1
#'
#'
#' # --------------------------------------------------------------------------
#' # One-way frequency table with arbitrary cconfidence intervals
#' #   - The default behavior of freq_table is to return 95% confidence
#' #     intervals (two-sided). However, this behavior can be adjusted to return
#' #     any alpha level. For example, to return 99% confidence intervals just
#' #     pass 99 to the percent_ci parameter of freq_table as demonstrated below.
#' # --------------------------------------------------------------------------
#' mtcars %>%
#'   freq_table(am, percent_ci = 99)
#'
#' #   A tibble: 2 x 9
#' #   var   cat       n n_total percent    se t_crit   lcl   ucl
#' #   <chr> <chr> <int>   <int>   <dbl> <dbl>  <dbl> <dbl> <dbl>
#' # 1 am    0        19      32    59.4  8.82   2.74  34.9  79.9
#' # 2 am    1        13      32    40.6  8.82   2.74  20.1  65.1
#'
#'
#' # --------------------------------------------------------------------------
#' # One-way frequency table with Wald confidence intervals
#' # Optionally, the ci_type = "wald" argument can be used to calculate Wald
#' # confidence intervals that match those returned by SAS.
#' # --------------------------------------------------------------------------
#' mtcars %>%
#'   freq_table(am, ci_type = "wald")
#'
#' #   A tibble: 2 x 9
#' #   var   cat       n n_total percent    se t_crit   lcl   ucl
#' #   <chr> <chr> <int>   <int>   <dbl> <dbl>  <dbl> <dbl> <dbl>
#' # 1 am    0        19      32    59.4  8.82   2.04  41.4  77.4
#' # 2 am    1        13      32    40.6  8.82   2.04  22.6  58.6
#'
#'
#' # --------------------------------------------------------------------------
#' # One-way frequency table with drop = FALSE (default)
#' # --------------------------------------------------------------------------
#' df <- data.frame(
#'   id = c(1, 2, 3, 4),
#'   gender = factor(
#'     # All females
#'     c(1, 1, 1, 1),
#'     levels = c(1, 2),
#'     labels = c("female", "male"))
#' )
#'
#' df %>%
#'   freq_table(gender)
#'
#' #   A tibble: 2 x 9
#' #   var    cat        n n_total percent    se t_crit   lcl   ucl
#' #   <chr>  <chr>  <int>   <int>   <dbl> <dbl>  <dbl> <dbl> <dbl>
#' # 1 gender female     4       4     100     0   3.18   NaN   NaN
#' # 2 gender male       0       4       0     0   3.18   NaN   NaN
#'
#'
#' # --------------------------------------------------------------------------
#' # One-way frequency table with drop = TRUE
#' # --------------------------------------------------------------------------
#' df <- data.frame(
#'   id = factor(rep(1:3, each = 4)),
#'   period = factor(rep(1:4)),
#'   x = factor(c(0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1))
#' )
#'
#' # Now, supppose we want to drop period 3 & 4 from our analysis.
#' # By default, this will give us 0s for period 3 & 4, but we want to drop them.
#'
#' df <- df %>%
#'   filter(period %in% c(1, 2))
#'
#' df %>%
#'   freq_table(period)
#'
#' #   A tibble: 4 x 9
#' #   var    cat       n n_total percent    se t_crit    lcl   ucl
#' #   <chr>  <chr> <int>   <int>   <dbl> <dbl>  <dbl>  <dbl> <dbl>
#' # 1 period 1         3       6      50  22.4   2.57   9.12  90.9
#' # 2 period 2         3       6      50  22.4   2.57   9.12  90.9
#' # 3 period 3         0       6       0   0     2.57 NaN    NaN
#' # 4 period 4         0       6       0   0     2.57 NaN    NaN
#'
#' # But, we don't want period 3 & 4 in our frequency table at all. That's
#' # when we should change drop to TRUE.
#'
#' df %>%
#'   freq_table(period, drop = TRUE)
#'
#' #   A tibble: 4 x 9
#' #   var    cat       n n_total percent    se t_crit    lcl   ucl
#' #   <chr>  <chr> <int>   <int>   <dbl> <dbl>  <dbl>  <dbl> <dbl>
#' # 1 period 1         3       6      50  22.4   2.57   9.12  90.9
#' # 2 period 2         3       6      50  22.4   2.57   9.12  90.9
#'
#'
#' # --------------------------------------------------------------------------
#' # Two-way frequency table with defaults
#' # Output truncated to fit the screen
#' # --------------------------------------------------------------------------
#' mtcars %>%
#'   freq_table(am, cyl)
#'
#' #   A tibble: 6 x 17
#' #   row_var row_cat col_var col_cat     n n_row n_total percent_total se_total
#' #   <chr>   <chr>   <chr>   <chr>   <int> <int>   <int>         <dbl>    <dbl>
#' # 1 am      0       cyl     4           3    19      32          9.38     5.24
#' # 2 am      0       cyl     6           4    19      32         12.5      5.94
#' # 3 am      0       cyl     8          12    19      32         37.5      8.70
#' # 4 am      1       cyl     4           8    13      32         25        7.78
#' # 5 am      1       cyl     6           3    13      32          9.38     5.24
#' # 6 am      1       cyl     8           2    13      32          6.25     4.35
freq_table <- function(.data, ..., percent_ci = 95, ci_type = "logit", drop = FALSE) {

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
  col_var = col_cat = `.` = vars = alpha = NULL

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
  .data <- dplyr::count(.data, ..., .drop = drop)

  # ===========================================================================
  # Check for number of group vars:
  # ===========================================================================
  n_groups <- .data %>% ncol() - 1
  if (n_groups > 2) {
    stop("Currently, freq_table accepts one or two variables -- not more. You entered ",
         n_groups, " into the ... argument.")
  }

  # ===========================================================================
  # Convert percent_95 to t_prob
  # 2020-02-14: Previously, t_prob was an argument to freq_table and was passed
  # directly to stats::qt(). However, t_prob is not necessarily intuitive to
  # many users. Therefore, they will not enter, for example, 95 as an argument
  # to the percent_95 parameter and that will be converted to a t_prob of 0.975
  # as t_prob = 1 - (percent_ci / 100)/2
  # ===========================================================================
  alpha <-  1 - (percent_ci / 100)
  t_prob <- 1 - alpha / 2

  # ===========================================================================
  # One-way tables
  # ===========================================================================
  if (n_groups == 1) {

    # Create first three columns of summary table: grouped variable name,
    # grouped variable categories, and n of each category
    out <- .data %>%
      dplyr::mutate(var = !!names(.[1])) %>%
      dplyr::rename(cat = !!names(.[1])) %>%
      dplyr::select(var, cat, n) %>%
      # Coerce all variable names and categories (i.e., 0 and 1) to character
      dplyr::mutate_at(dplyr::vars(-n), as.character)

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
      # Coerce all variable names and categories (i.e., 0 and 1) to character
      dplyr::mutate_at(dplyr::vars(-n), as.character) %>%

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
