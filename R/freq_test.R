#' @title Hypothesis Testing for Frequency Tables
#'
#' @description The freq_test function is an S3 generic. It currently has
#'   methods for conducting hypothesis tests on one-way and two-way frequency
#'   tables. It is designed to work in a dplyr pipeline with the freq_table
#'   function.
#'
#'   For the freq_table_one_way class, the method used is Pearson's chi-square
#'   test for equality of proportions.
#'
#'   For the freq_table_two_way class, the methods used are Pearson's
#'   chi-square test of independence and Fisher's exact test (when any
#'   expected cell count is <= 5).
#'
#' @param .data A tibble of class freq_table_one_way or freq_table_two_way.
#'
#' @param ... Other parameters to be passed on.
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
#' mtcars %>%
#'   freq_table(am) %>%
#'   freq_test()
#'
#' # Chi-square test of independence
#' mtcars %>%
#'   group_by(cyl) %>%
#'   freq_table(am) %>%
#'   freq_test()

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

  # ------------------------------------------------------------------
  # Prevents R CMD check: "no visible binding for global variable '.'"
  # ------------------------------------------------------------------
  n = n_expected = chi2_contrib = pchisq = chi2_pearson = df = . = NULL

  # Check to make sure .data is a freq_table_one_way
  if (!("freq_table_one_way" %in% class(.data))) {
    stop(".data must be of class freq_table_one_way. It is currently: ",
         paste(class(.data), collapse = ", "))
  }

  # Calculate chi-square test of equality
  out <- .data %>%
    dplyr::mutate(
      n_expected     = sum(n) / nrow(.),
      chi2_contrib   = (n - n_expected)**2 / n_expected,
      chi2_pearson   = sum(chi2_contrib),
      df             = nrow(.) - 1,
      p_chi2_pearson = pchisq(chi2_pearson, df, lower.tail = FALSE)
    )

  # Add class to out
  class(out) <- c("freq_table_one_way", class(out))

  # Preserve attributes
  attr(out, "col_var") <- attr(.data, "col_var")
  attr(out, "group_vars") <- attr(.data, "group_vars")

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

  # ------------------------------------------------------------------
  # Prevents R CMD check: "no visible binding for global variable '.'"
  # ------------------------------------------------------------------
  n_group = n_col = n_total = n_expected = chi2_contrib = r = pchisq = NULL
  chi2_pearson = df = n = NULL

  # Check to make sure .data is a freq_table_two_way
  if (!("freq_table_two_way" %in% class(.data))) {
    stop(".data must be of class freq_table_two_way. It is currently: ",
         paste(class(.data), collapse = ", "))
  }

  # ===========================================================================
  # Identify column structure from attributes
  # ===========================================================================
  col_var_name <- attr(.data, "col_var")
  grp_vars <- attr(.data, "group_vars")

  if (is.null(col_var_name) || is.null(grp_vars)) {
    stop(
      "Cannot determine column structure. ",
      "Make sure .data was created by freq_table()."
    )
  }

  # ===========================================================================
  # Compute chi-square test of independence
  # ===========================================================================

  # Number of unique row (group) and column (analysis) categories
  if (length(grp_vars) == 1) {
    grp_keys <- as.character(.data[[grp_vars[1]]])
  } else {
    grp_keys <- do.call(paste, c(.data[grp_vars], sep = "_"))
  }
  col_cats <- as.character(.data[[col_var_name]])

  r_val <- length(unique(grp_keys))
  c_val <- length(unique(col_cats))

  # Total n
  n_total_val <- sum(.data$n)

  # Column marginal totals: sum of n for each level of the analysis variable
  col_total_lookup <- tapply(.data$n, col_cats, sum)
  n_col_vec <- as.numeric(col_total_lookup[col_cats])

  # Calculate chi-square statistics
  out <- .data
  out$n_col <- n_col_vec
  out$n_total <- n_total_val
  out$n_expected <- (out$n_group * out$n_col) / out$n_total
  out$chi2_contrib <- (out$n - out$n_expected)**2 / out$n_expected
  chi2_val <- sum(out$chi2_contrib)
  out$chi2_pearson <- chi2_val
  out$r <- r_val
  out$c <- c_val
  df_val <- (r_val - 1) * (c_val - 1)
  out$df <- df_val
  out$p_chi2_pearson <- stats::pchisq(chi2_val, df_val, lower.tail = FALSE)

  # Test for expected cell counts <= 5
  if (min(out$n_expected) <= 5) {
    message(paste0(
      "One or more expected cell counts are <= 5. Therefore, ",
      "Fisher's Exact Test was used."
    ))

    # Build contingency table matrix for Fisher's test
    mx <- matrix(.data$n, nrow = r_val, byrow = TRUE)

    # Use R's built-in fisher.test
    fisher <- stats::fisher.test(mx)
    out$p_fisher <- fisher$p.value
  }

  # Add class
  class(out) <- c("freq_table_two_way", class(out))

  # Preserve attributes
  attr(out, "col_var") <- col_var_name
  attr(out, "group_vars") <- grp_vars

  out
}
