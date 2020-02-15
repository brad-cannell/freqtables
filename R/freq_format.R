#' Format freq_table Output for Publication and Dissemination
#'
#' @description The freq_format function is intended to make it quick and easy to
#'   format the output of the freq_table function for tables that may be used
#'   for publication. For example, a proportion and 95% confidence interval
#'   could be formatted as "24.00 (21.00 - 27.00)."
#'
#' @param .data A data frame of class "freq_table_one_way" or "freq_table_two_way".
#' @param recipe A recipe used to create a new column from existing freq_table
#'   columns. The recipe must be in the form of a quoted string. It may contain
#'   any combination of column names, spaces, and characters. For example:
#'   "n (percent)" or "percent (lcl - ucl)".
#' @param name An optional name to assign to the column created by the recipe.
#'   The default name is "formatted_stats"
#' @param digits The number of decimal places to display.
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
#'   freq_format(
#'     recipe = "percent (lcl - ucl)",
#'     name = "percent_95",
#'     digits = 2
#'   ) %>%
#'   select(var, cat, percent_95)
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
#'     freq_format(
#'     recipe = "percent_row (lcl_row - ucl_row)",
#'     name = "percent_95",
#'     digits = 2
#'   ) %>%
#'   select(1:4, percent_95)
#' #> # A tibble: 6 x 5
#' #>   row_var row_cat col_var col_cat percent_95
#' #>   <chr>   <chr>   <chr>   <chr>   <chr>
#' #> 1 am      0       cyl     4       15.79 (4.78 - 41.20)
#' #> 2 am      0       cyl     6       21.05 (7.58 - 46.44)
#' #> 3 am      0       cyl     8       63.16 (38.76 - 82.28)
#' #> 4 am      1       cyl     4       61.54 (32.30 - 84.29)
#' #> 5 am      1       cyl     6       23.08 (6.91 - 54.82)
#' #> 6 am      1       cyl     8       15.38 (3.43 - 48.18)
freq_format <- function(.data, recipe, name = NA, digits = NA) {

  # ===========================================================================
  # Prevents R CMD check: "no visible binding for global variable ‘.’"
  # ===========================================================================
  # name = recipe = ingredients = stat = NULL

  # ===========================================================================
  # Check function arguments
  # ===========================================================================
  # If no name given, default to formatted_stats
  if(is.na(name)) {
    name <- "formatted_stats"
  }

  # Break up the recipe into its component pieces
  # [1] "" "n" " (" "percent" ")"
  recipe <- stringr::str_split(recipe, "\\b")
  # First component is always an empty string. Drop it.
  recipe <- unlist(recipe)[-1]

  # Loop over each row of the freq_table
  for(i in seq(nrow(.data))) {
    # Empty vector to hold the stats and symbols that will make up the new
    # variable in that row. The ingredients for the recipe.
    ingredients <- c()
    # Loop over each component of the recipe
    # [1] "n" " (" "percent" ")"
    for(j in seq_along(recipe)) {
      # If that the component is a column in the data frame then grab the value
      # for that column in that row and add it to the ingredients.
      if(recipe[j] %in% names(.data)){
        # Get the stat (e.g., n or percent)
        stat <- .data[[recipe[j]]][i]
        # Round the stat if an argument is supplied to the digits argument
        if(!is.na(digits)) {
          # But don't add trailing zeros to integers
          if(!is.integer(stat)) {
            stat <- round(stat, digits)
            stat <- format(stat, nsmall = digits, big.mark = ",")
            stat <- trimws(stat)
          }
        }
        ingredients <- c(ingredients, stat)
        # If that component is not a column in .data then add it to the
        # ingredients vector as a character.
      } else {
        ingredients <- c(ingredients, recipe[j])
      }
    }
    # Add the new variable to the .data
    .data[i, name] <- paste(ingredients, collapse = "")
  }

  # Return .data with the new variable
  .data
}
