#' Format statistics for presentation
#'
#' Append a character column composed from existing table columns and literal
#' text. Works with compact proportions, percentages, grouped results, and
#' [freq_test()] output. Calculations retain full precision; rounding happens
#' only in the new display column.
#'
#' @param .data A data frame, commonly from [freq_tbl()] or [freq_table()].
#' @param recipe One quoted string containing column-name tokens and literal
#'   text, e.g. `"n (percent%)"` or `"prop_group (lcl_group - ucl_group)"`.
#'   Tokens use word boundaries; names containing spaces/punctuation should
#'   be renamed first. Unmatched tokens remain literal text.
#' @param name Name of the appended column. Default `NA` selects
#'   `"formatted_stats"`. An existing column with this name is replaced.
#' @param digits Nonnegative integer number of decimal places. Default `NA`
#'   uses character conversion without explicit rounding. When supplied,
#'   doubles are rounded with trailing zeros and thousands separators;
#'   integer counts retain their integer representation. Character categories
#'   are not rounded.
#' @return The input data frame with one character column added or replaced.
#' @export
#' @examples
#' freq_table(mtcars, am, percent = TRUE) |>
#'   freq_format("percent (lcl - ucl)", name = "percent_ci", digits = 2)
#' mtcars |> dplyr::group_by(cyl) |> freq_table(am) |>
#'   freq_format("n: prop_group (lcl_group - ucl_group)", digits = 3)
freq_format <- function(.data, recipe, name = NA, digits = NA) {
  if (!is.data.frame(.data)) stop(".data must be a data frame.", call. = FALSE)
  if (!is.character(recipe) || length(recipe) != 1L || is.na(recipe)) {
    stop("recipe must be one nonmissing string.", call. = FALSE)
  }
  if (length(name) != 1L) stop("name must be one string or NA.", call. = FALSE)
  if (is.na(name)) name <- "formatted_stats"
  if (!is.character(name) || !nzchar(name)) {
    stop("name must be a nonempty string or NA.", call. = FALSE)
  }
  if (length(digits) != 1L ||
      (!is.na(digits) && (!is.numeric(digits) || !is.finite(digits) ||
                         digits < 0 || digits != floor(digits)))) {
    stop("digits must be a nonnegative integer or NA.", call. = FALSE)
  }
  pieces <- stringr::str_split(recipe, "\\b")[[1]]
  formatted <- vapply(seq_len(nrow(.data)), function(i) {
    ingredients <- vapply(pieces, function(piece) {
      if (!piece %in% names(.data)) return(piece)
      value <- .data[[piece]][i]
      if (!is.na(digits) && is.numeric(value) && !is.integer(value)) {
        return(trimws(format(round(value, digits), nsmall = digits,
                             big.mark = ",")))
      }
      as.character(value)
    }, character(1))
    paste(ingredients, collapse = "")
  }, character(1))
  .data[[name]] <- formatted
  .data
}
