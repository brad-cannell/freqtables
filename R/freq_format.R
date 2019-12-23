#' Format freq_table Output for Publication and Dissemination
#'
#' @description The freq_format is intended make it quick and easy to
#'   format the output of the freq_table function for tables that may be used
#'   for publication. For example, a proportion and 95% confidence interval
#'   could be formatted as "24.00 (21.00 - 27.00)."
#'
#' @param .data A data frame of class "freq_table_one_way" or "freq_table_two_way".
#' @param ... A recipe used to create a new column from existing freq_table columns.
#' @param name A name to assign to the column created by the recipe in ....
#' @param decimals The number of decimal places to display. Default value is 2.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' library(dplyr)
#' library(freqtables)
#'
#' data(mtcars)
#'
#' # One-way frequency tables with defaults
#'
#' mtcars %>%
#'   freq_table(am) %>%
#'   freq_format(name = "percent_95", percent, " (", lcl, " - ", ucl, ")")
#' #> # A tibble: 2 x 3
#' #>   var   cat   percent_95
#' #>   <chr> <chr> <chr>
#' #> 1 am    0     59.38 (40.94 - 75.50)
#' #> 2 am    1     40.62 (24.50 - 59.06)
#'
#' # Two-way frequency tables with defaults
#'
#' mtcars %>%
#'   freq_table(am, cyl) %>%
#'   freq_format(name = "percent_95", percent_row, " (", lcl_row, " - ", ucl_row, ")")
#'
#' #> # A tibble: 6 x 5
#' #>   row_var row_cat col_var col_cat percent_95
#' #>   <chr>   <chr>   <chr>   <chr>   <chr>
#' #> 1 am      0       cyl     4       15.79 (4.78 - 41.20)
#' #> 2 am      0       cyl     6       21.05 (7.58 - 46.44)
#' #> 3 am      0       cyl     8       63.16 (38.76 - 82.28)
#' #> 4 am      1       cyl     4       61.54 (32.30 - 84.29)
#' #> 5 am      1       cyl     6       23.08 (6.91 - 54.82)
#' #> 6 am      1       cyl     8       15.38 (3.43 - 48.18)
freq_format <- function(.data, ..., name = NA, decimals = 2) {

  # ===========================================================================
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ===========================================================================
  `:=` = var = row_var = row_cat = col_var = col_cat = vars = NULL

  # ===========================================================================
  # Variable setup
  # ===========================================================================
  .args  <- rlang::enquos(...)
  .name  <- rlang::quo_name(name)
  .class <- class(.data)

  # ===========================================================================
  # Format the existing columns in .data
  # ===========================================================================
  # Add comma to n, n_total, etc. when n > 999
  .data <- .data %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("n")),
      format, big.mark = ","
    )

  # For all other numeric variables
  #  - Round
  #  - Trim white space
  .data <- .data %>%
    dplyr::mutate_if(
      is.numeric,
      function(x) {
        # Round to x decimal places
        x <- round(x, decimals)
        # Show at least x decimal places
        x <- format(x, nsmall = decimals)
        # Remove leading/trailing whitespace
        x <- trimws(x)
        x
      }
    )

  # ===========================================================================
  # Create a new variable using the recipe in ...
  # ===========================================================================
  .data <- .data %>%
    dplyr::mutate(!!name := paste0(!!!.args))

  # ===========================================================================
  # Control output
  # ===========================================================================
  if("freq_table_one_way" %in% .class) {
    out <- .data %>%
      dplyr::select(var, cat, !!.name)
  } else if("freq_table_two_way" %in% .class) {
    out <- .data %>%
      dplyr::select(row_var, row_cat, col_var, col_cat, !!.name)
  }

  # Return result
  out
}
