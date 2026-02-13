#' @title Hypothesis Testing for Frequency Tables
#'
#' @description The freq_test function is an S3 generic. It currently has
#'   methods for conducting hypothesis tests on one-way and two-way frequency
#'   tables. Further, it is made to work in a dplyr pipeline with the
#'   freq_table function.
#'
#'   For the freq_table_two_way class, the methods used are Pearson's
#'   chi-square test of independence Fisher's exact test. When cell counts
#'   are <= 5, Fisher's Exact Test is considered more reliable.
#'
#' @param .data A tibble of class freq_table_one_way or freq_table_two_way.
#'
#' @param ... Other parameters to be passed on.
#'
#' @param method Options for this parameter control the method used to
#'   calculate p-values.
#'

#'
#' @return A tibble.
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' library(dplyr)
#' library(freqtables)
#'
#' data(mtcars)
#'
#' # Test equality of proportions
#'
#' mtcars %>%
#'   freq_table(am) %>%
#'   freq_test() %>%
#'   select(var:percent, p_chi2_pearson)
#'
#' #>  # A tibble: 2 x 6
#' #>      var   cat     n n_total percent p_chi2_pearson
#' #>    <chr> <dbl> <int>   <int>   <dbl>          <dbl>
#' #>  1    am     0    19      32   59.38      0.2888444
#' #>  2    am     1    13      32   40.62      0.2888444
#'
#' # Chi-square test of independence
#'
#' mtcars %>%
#'   freq_table(am, vs) %>%
#'   freq_test() %>%
#'   select(row_var:n, percent_row, p_chi2_pearson)
#'
#' #> # A tibble: 4 x 7
#' #>   row_var row_cat col_var col_cat     n percent_row p_chi2_pearson
#' #>     <chr>   <dbl>   <chr>   <dbl> <int>       <dbl>          <dbl>
#' #> 1      am       0      vs       0    12       63.16      0.3409429
#' #> 2      am       0      vs       1     7       36.84      0.3409429
#' #> 3      am       1      vs       0     6       46.15      0.3409429
#' #> 4      am       1      vs       1     7       53.85      0.3409429

# =============================================================================
# S3 Generic function
# =============================================================================
freq_test <- function(.data, ...) {
  UseMethod("freq_test")
}




# =============================================================================
# Method for class freq_table_one_way
# Chi-square test for equal proportions
# =============================================================================
#' @export
#' @rdname freq_test

freq_test.freq_table_one_way <- function(.data, ...) {

  n = n_total = n_expected = chi2_contrib = pchisq = chi2_pearson = df = . = NULL

  if (!("freq_table_one_way" %in% class(.data))) {
    stop(".data must be of class freq_table_one_way. It is currently: ", class(.data))
  }

  if (!("n_total" %in% names(.data))) {
    stop("freq_test() for one-way tables requires an `n_total` column.")
  }

  out <- .data %>%
    dplyr::mutate(
      n_expected     = n_total / nrow(.),
      chi2_contrib   = (n - n_expected)**2 / n_expected,
      chi2_pearson   = sum(chi2_contrib),
      df             = nrow(.) - 1,
      p_chi2_pearson = pchisq(chi2_pearson, df, lower.tail = FALSE)
    )

  class(out) <- c("freq_table_one_way", class(out))
  out
}




# =============================================================================
# Method for class freq_table_two_way
# Pearson's Chi-square test for independence
# Fisher's exact test for independence
# =============================================================================
#' @export
#' @rdname freq_test

freq_test.freq_table_two_way <- function(.data, ...) {

  n = n_col = n_group = n_expected = chi2_contrib = r = pchisq = NULL
  chi2_pearson = df = col_cat = group_cat = NULL

  if (!("freq_table_two_way" %in% class(.data))) {
    stop(".data must be of class freq_table_two_way. It is currently: ", class(.data))
  }

  if (!("n_group" %in% names(.data))) {
    stop("freq_test() for grouped tables requires an `n_group` column.")
  }

  group_col <- names(.data)[1]
  outcome_col <- names(.data)[2]

  out <- .data %>%
    dplyr::rename(group_cat = !!group_col, col_cat = !!outcome_col) %>%
    dplyr::mutate(
      group_cat = as.character(group_cat),
      col_cat = as.character(col_cat)
    ) %>%
    dplyr::group_by(col_cat) %>%
    dplyr::mutate(n_col = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      n_expected     = (n_group * n_col) / sum(n),
      chi2_contrib   = (n - n_expected)**2 / n_expected,
      chi2_pearson   = sum(chi2_contrib),
      r              = dplyr::n_distinct(group_cat),
      c              = dplyr::n_distinct(col_cat),
      df             = (r - 1) * (c - 1),
      p_chi2_pearson = pchisq(chi2_pearson, df, lower.tail = FALSE)
    )

  if (min(out$n_expected) <= 5) {
    mx <- xtabs(n ~ group_cat + col_cat, data = out)
    fisher <- stats::fisher.test(mx)
    out <- out %>% dplyr::mutate(p_fisher = fisher$p.value)
  }

  class(out) <- c("freq_table_two_way", class(out))
  out
}
