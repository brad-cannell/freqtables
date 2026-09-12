#' @title Formatted Group Sample Size for Tables
#'
#' @description Given a tibble and a filter expression, get_group_n returns
#'   the group sample size formatted as "N = XXXX". Made to work in a dplyr
#'   pipeline, and used when creating tables for publications / reports.
#'
#' @param .data A local data frame or tibble, optionally grouped. Grouped
#'   input returns one label per retained group, in dplyr group order.
#' @param ... Zero or more dplyr::filter expressions, combined with AND.
#'   With no expressions, count all input rows. Missing logical results are
#'   excluded by filter(). Expressions may use dplyr data masking.
#'
#' @return A character vector of formatted counts. Ungrouped input returns
#'   one label, including "N = 0" when no observations remain.
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' library(dplyr)
#' library(freqtables)
#'
#' data(mtcars)
#'
#' # Get sample size for cars with 4 cylinders
#' mtcars %>% get_group_n(cyl == 4)
#'
#' #> [1] "N = 11"
get_group_n <- function(.data, ...) {

  # ------------------------------------------------------------------
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ------------------------------------------------------------------
  n = NULL

  # Turn filter expression into a quoture
  filter_exp <- rlang::quos(...)

  # Filter .data by the filter expression (e.g., cyl == 4)
  # Count number of remaining rows
  # Format number of rows as: N = XX
  # Return as a character vector
  .data %>%
    dplyr::filter(!!!filter_exp) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(n = paste0("N = ", format(n, big.mark = ","))) %>%
    dplyr::pull(n)
}
