#' Compact counts and proportions
#'
#' Count one outcome within zero, one, or multiple groups supplied by
#' [dplyr::group_by()]. Each observation contributes once, including missing
#' outcomes and missing group values. Use [dplyr::filter()] before counting to
#' exclude missing observations explicitly.
#'
#' @param .data A local data frame or grouped data frame. Rowwise data is not
#'   supported; call [dplyr::ungroup()] first.
#' @param .freq_var One unquoted column name (or a string naming a column).
#'   In a wrapper function, use `{{ variable }}`.
#' @param ... Reserved for detecting obsolete multiple-outcome calls and
#'   misspelled options. Must be empty; options must be named.
#' @param drop Logical. If `FALSE` (default), retain unobserved factor levels
#'   and empty factor groups with zero counts. If `TRUE`, retain observed
#'   combinations only. Does not remove missing values.
#' @param percent Logical. If `FALSE`, return proportions in `prop`,
#'   `prop_group`, and (when requested) `prop_total`. If `TRUE`, use
#'   `percent`, `percent_group`, and `percent_total` on the 0--100 scale.
#' @param overall Logical. For grouped data, append `n_total` and
#'   `prop_total` (or `percent_total`): the cell count divided by the total
#'   input rows, not the outcome's marginal proportion. For ungrouped data,
#'   append only `n_total`; `prop` is already the overall proportion.
#' @param generic_col_names Logical. If `TRUE`, replace each group column
#'   with `group_01_col`/`group_01_cat`, etc., and the outcome with
#'   `col`/`cat`. Category columns become character for stacking tables.
#'
#' @return An ungrouped tibble of class `freq_tbl`. Ungrouped defaults are
#'   outcome, `n`, `prop`; grouped defaults are group columns, outcome,
#'   `n`, `n_group`, `prop_group`. `n_group` is the total number of
#'   observations in that group, across all outcome categories. Zero
#'   denominators produce `NA_real_` proportions. Variable types are preserved
#'   unless generic headers are requested. Metadata retains the original total
#'   for subsequent confidence-interval calculations even after filtering rows.
#'   Analysis column names that collide with generated columns are rejected;
#'   rename them before calling.
#' @export
#' @examples
#' freq_tbl(mtcars, am)
#' mtcars |> dplyr::group_by(cyl) |> freq_tbl(am)
#' mtcars |> dplyr::group_by(cyl, vs) |> freq_tbl(am, percent = TRUE)
#' freq_tbl(mtcars, am, generic_col_names = TRUE, overall = TRUE)
freq_tbl <- function(.data, .freq_var, ..., drop = FALSE, percent = FALSE,
                     overall = FALSE, generic_col_names = FALSE) {
  check_frequency_input(.data, rlang::enquo(.freq_var), rlang::enquos(...),
                        "freq_tbl")
  for (option in c("drop", "percent", "overall", "generic_col_names")) {
    check_flag(get(option), option)
  }
  outcome <- rlang::as_name(rlang::enquo(.freq_var))
  groups <- dplyr::group_vars(.data)
  variables <- c(groups, outcome)
  reserved <- c("n", "n_group", "n_total", "prop", "prop_group", "prop_total",
                "percent", "percent_group", "percent_total",
                outer(c("se", "t_crit", "lcl", "ucl"),
                      c("", "_group", "_total"), paste0))
  if (generic_col_names) {
    reserved <- c(reserved, "col", "cat",
                  sprintf("group_%02d_col", seq_along(groups)),
                  sprintf("group_%02d_cat", seq_along(groups)))
  }
  collisions <- intersect(variables, reserved)
  if (length(collisions)) {
    stop("Rename analysis columns that conflict with result names: ",
         paste(collisions, collapse = ", "), ".", call. = FALSE)
  }

  # Select explicitly so a pre-existing, unrelated input column named n cannot
  # be interpreted as a weight by count().
  input <- dplyr::select(dplyr::ungroup(.data), dplyr::all_of(variables))
  out <- dplyr::count(input, !!!rlang::syms(variables),
                      name = "n", .drop = drop)
  out <- dplyr::as_tibble(out)
  total <- nrow(.data)
  if (length(groups)) {
    out <- dplyr::group_by(out, !!!rlang::syms(groups), .drop = drop)
    out <- dplyr::mutate(out, n_group = sum(.data$n))
    out <- dplyr::ungroup(out)
    out$prop_group <- safe_proportion(out$n, out$n_group)
  } else {
    out$prop <- safe_proportion(out$n, total)
  }
  if (overall) {
    out$n_total <- rep(total, nrow(out))
    if (length(groups)) out$prop_total <- safe_proportion(out$n, total)
  }
  if (generic_col_names) {
    generic <- list()
    for (i in seq_along(groups)) {
      generic[[sprintf("group_%02d_col", i)]] <- rep(groups[i], nrow(out))
      generic[[sprintf("group_%02d_cat", i)]] <- as.character(out[[groups[i]]])
    }
    generic$col <- rep(outcome, nrow(out))
    generic$cat <- as.character(out[[outcome]])
    out <- dplyr::bind_cols(dplyr::as_tibble(generic),
                           out[setdiff(names(out), variables)])
  }
  if (percent) {
    for (column in intersect(c("prop", "prop_group", "prop_total"), names(out))) {
      out[[column]] <- out[[column]] * 100
      names(out)[names(out) == column] <- sub("^prop", "percent", column)
    }
  }
  attr(out, "freqtables") <- list(
    outcome = outcome, groups = groups, n_total = total,
    percent = percent, generic_col_names = generic_col_names
  )
  class(out) <- c("freq_tbl", class(out))
  out
}

check_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop(name, " must be TRUE or FALSE.", call. = FALSE)
  }
}

check_frequency_input <- function(data, variable, dots, caller) {
  if (!is.data.frame(data)) {
    stop(caller, "() expects .data to be a data frame.", call. = FALSE)
  }
  if (inherits(data, "rowwise_df")) {
    stop("Rowwise data is not supported; call dplyr::ungroup() first.",
         call. = FALSE)
  }
  if (length(dots)) {
    stop(caller, "() accepts one outcome and named options only. ",
         "For grouped analysis use mtcars |> dplyr::group_by(cyl) |> ",
         caller, "(am), instead of ", caller, "(mtcars, cyl, am). ",
         "Unused options: ", paste(vapply(dots, rlang::as_label, character(1)),
                                    collapse = ", "), ".", call. = FALSE)
  }
  if (rlang::quo_is_missing(variable)) {
    stop(caller, "() requires one column in .freq_var.", call. = FALSE)
  }
  expression <- rlang::quo_get_expr(variable)
  if (!(rlang::is_symbol(expression) || rlang::is_string(expression))) {
    stop(".freq_var must name one existing column; create derived columns ",
         "with dplyr::mutate() first.", call. = FALSE)
  }
  outcome <- rlang::as_name(variable)
  if (!outcome %in% names(data)) {
    stop("Column '", outcome, "' is not in .data.", call. = FALSE)
  }
  if (outcome %in% dplyr::group_vars(data)) {
    stop("The outcome must not also be a grouping variable.", call. = FALSE)
  }
  if (anyDuplicated(names(data)) || anyNA(names(data)) || any(names(data) == "")) {
    stop(".data must have unique, nonempty column names.", call. = FALSE)
  }
}

safe_proportion <- function(count, denominator) {
  out <- count / denominator
  out[!is.finite(out)] <- NA_real_
  out
}
