#' Create multiple frequency tables
#'
#' @param .data A data frame (optionally grouped).
#' @param ... One or more outcome variables.
#' @param .freq_table_args Named arguments passed to `freq_table()`.
#' @return A tibble with one row per variable and a list-column of tables.
#' @export
freq_tables <- function(.data, ..., .freq_table_args = list()) {
  vars <- rlang::enquos(...)
  if (length(vars) < 1) {
    stop("freq_tables() expects at least one variable.")
  }

  out_list <- lapply(vars, function(v) {
    rlang::inject(freq_table(.data = .data, !!v, !!!.freq_table_args))
  })

  dplyr::tibble(
    col = vapply(vars, rlang::as_name, character(1)),
    freq_table = out_list
  )
}
