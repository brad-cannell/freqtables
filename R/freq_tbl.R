#' Create grouped frequency counts and proportions
#'
#' @param .data A data frame (optionally grouped with dplyr::group_by()).
#' @param ... A single categorical outcome variable.
#' @param percent If TRUE, return proportions on the percent scale and rename
#'   `prop*` columns to `percent*`.
#' @param overall If TRUE, add overall n and proportion columns.
#' @param generic_col_names If TRUE, use generic columns (`group_01_col`,
#'   `group_01_cat`, `col`, `cat`) instead of data column names.
#' @param drop Passed to dplyr::count(.drop = ).
#' @param digits Number of decimal places displayed for `prop*`/`percent*`
#'   columns. Must be a non-negative integer.
#' @return A tibble.
#' @export
freq_tbl <- function(.data,
                     ...,
                     percent = FALSE,
                     overall = FALSE,
                     generic_col_names = FALSE,
                     drop = FALSE,
                     digits = 2) {

  outcome_quos <- rlang::enquos(...)
  if (length(outcome_quos) != 1) {
    stop(
      "freq_tbl() expects exactly one analysis variable. Use dplyr::group_by() for grouping, e.g. `df |> group_by(sex) |> freq_tbl(diabetes)`."
    )
  }

  if (!inherits(.data, "data.frame")) {
    stop("freq_tbl expects `.data` to be a data frame.")
  }
  if (!is.null(digits) &&
      (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
       digits < 0 || digits != as.integer(digits))) {
    stop("`digits` must be a single non-negative integer (or NULL).")
  }

  group_vars <- dplyr::group_vars(.data)
  outcome_name <- rlang::as_name(outcome_quos[[1]])
  count_vars <- c(group_vars, outcome_name)

  out <- dplyr::count(.data, !!!rlang::syms(count_vars), .drop = drop)

  if (length(group_vars) == 0) {
    out <- dplyr::mutate(
      out,
      n_total = sum(n),
      prop = n / n_total
    )
    if (!overall) {
      out <- dplyr::select(out, !!rlang::sym(outcome_name), n, prop)
    }
  } else {
    out <- out |>
      dplyr::group_by(!!!rlang::syms(group_vars)) |>
      dplyr::mutate(
        n_group = sum(n),
        prop_group = n / n_group
      ) |>
      dplyr::ungroup()

    if (overall) {
      out <- dplyr::mutate(
        out,
        n_total = sum(n),
        prop_total = n / n_total
      )
    }
  }

  if (generic_col_names) {
    if (length(group_vars) > 0) {
      for (i in seq_along(group_vars)) {
        g_col <- group_vars[[i]]
        out[[sprintf("group_%02d_col", i)]] <- g_col
        out[[sprintf("group_%02d_cat", i)]] <- as.character(out[[g_col]])
      }
    }

    out$col <- outcome_name
    out$cat <- as.character(out[[outcome_name]])

    keep <- c(
      unlist(lapply(seq_along(group_vars), function(i) sprintf("group_%02d_col", i))),
      unlist(lapply(seq_along(group_vars), function(i) sprintf("group_%02d_cat", i))),
      "col", "cat",
      setdiff(names(out), c(group_vars, outcome_name))
    )
    out <- dplyr::select(out, dplyr::all_of(keep))
  }

  if (percent) {
    prop_cols <- names(out)[grepl("^prop", names(out))]
    if (length(prop_cols) > 0) {
      out <- dplyr::mutate(out, dplyr::across(dplyr::all_of(prop_cols), ~ .x * 100))
      out <- dplyr::rename_with(
        out,
        ~ sub("^prop", "percent", .x),
        dplyr::all_of(prop_cols)
      )
    }
  }

  display_cols <- names(out)[grepl("^prop|^percent", names(out))]
  if (!is.null(digits) && length(display_cols) > 0) {
    out <- dplyr::mutate(
      out,
      dplyr::across(dplyr::all_of(display_cols), ~ pillar::num(.x, digits = digits))
    )
  }

  out
}
