#' Filter Out NA
#'
#' @description Filter out (i.e., remove) rows of .data that contain a value of
#' NA for each column name passed to `...`. Must be used before `group_by()`.
#'
#' @param .data The data frame to be analyzed
#' @param ... The column names to check for NA values
#'
#' @return
#' @export
#'
#' @examples
filter_na <- function(.data, ...) {
  .data %>%
    dplyr::filter(dplyr::if_all(
      .cols = c(!!!rlang::enquos(...)),
      .fns  = ~ !is.na(.x)
    ))
}

# For testing
# data(freq_study)
# devtools::load_all()
# freq_study %>% filter_na(sex) # Should be 99
# freq_study %>% filter_na(exposure, sex) # Should be 98

# Improve help documentation
# Check for no vars passed to ...
# Make informative error if the use filter_na after group_by.
