#' Hypothesis tests for frequency tables
#'
#' Test equal category probabilities for an ungrouped table, or independence
#' between one grouping variable and the outcome for a two-way table.
#' Pearson's statistic is uncorrected (no Yates correction). For a two-way
#' table with any expected count <= 5, also return Fisher's exact p-value and
#' emit a message. The table is constructed from category identities, with
#' omitted cells filled with zero, so row order and non-binary groups work.
#'
#' @param .data A complete `freq_table_one_way` or `freq_table_two_way`
#'   result from [freq_table()], with its metadata and category/count columns
#'   intact. Missing values count as categories unless filtered before making
#'   the table. Tables stacked from different outcomes are not supported.
#'   Two-way tests require at least two observed categories on each axis and
#'   no zero marginal totals; use `drop = TRUE` to omit empty factor levels.
#' @param ... Reserved; no extra arguments are currently supported by either
#'   method. Nonempty `...` produces an error rather than being ignored.
#' @return The input with expected counts, per-cell chi-square contributions,
#'   the overall Pearson statistic, degrees of freedom, and
#'   `p_chi2_pearson`. Two-way output also has `n_col`, `r`, `c`, and
#'   (when expected counts are small) `p_fisher`. Omitted zero-count cells
#'   contribute to the statistic even though they are absent from the output.
#'   Fisher's exact calculation can fail for very large tables; the error
#'   from [stats::fisher.test()] is propagated.
#' @export
#' @examples
#' freq_table(mtcars, am) |> freq_test()
#' mtcars |> dplyr::group_by(am) |> freq_table(cyl) |> freq_test()
#' mtcars |> dplyr::group_by(cyl) |>
#'   freq_table(am, generic_col_names = TRUE) |> freq_test()
freq_test <- function(.data, ...) {
  UseMethod("freq_test")
}

#' @rdname freq_test
#' @export
freq_test.freq_table_one_way <- function(.data, ...) {
  check_test_input(.data, rlang::enquos(...))
  if (nrow(.data) < 2L || sum(.data$n) <= 0) {
    stop("A one-way test requires at least two categories and a positive total.",
         call. = FALSE)
  }
  expected <- sum(.data$n) / nrow(.data)
  .data$n_expected <- rep(expected, nrow(.data))
  .data$chi2_contrib <- (.data$n - expected)^2 / expected
  .data$chi2_pearson <- rep(sum(.data$chi2_contrib), nrow(.data))
  .data$df <- rep(nrow(.data) - 1L, nrow(.data))
  .data$p_chi2_pearson <- stats::pchisq(.data$chi2_pearson, .data$df,
                                      lower.tail = FALSE)
  .data
}

#' @rdname freq_test
#' @export
freq_test.freq_table_two_way <- function(.data, ...) {
  meta <- check_test_input(.data, rlang::enquos(...))
  group_column <- if (meta$generic_col_names) "group_01_cat" else meta$groups[1]
  outcome_column <- if (meta$generic_col_names) "cat" else meta$outcome
  row_id <- match(.data[[group_column]], unique(.data[[group_column]]))
  col_id <- match(.data[[outcome_column]], unique(.data[[outcome_column]]))
  nr <- max(c(0L, row_id))
  nc <- max(c(0L, col_id))
  if (nr < 2L || nc < 2L) {
    stop("A two-way test requires at least two categories on each axis.",
         call. = FALSE)
  }
  index <- cbind(row_id, col_id)
  counts <- matrix(0, nrow = nr, ncol = nc)
  counts[index] <- .data$n
  row_n <- rowSums(counts)
  col_n <- colSums(counts)
  if (any(row_n == 0) || any(col_n == 0)) {
    stop("Two-way tests require positive marginal totals; use drop = TRUE ",
         "or remove empty categories before testing.", call. = FALSE)
  }
  expected <- outer(row_n, col_n) / sum(counts)
  contributions <- (counts - expected)^2 / expected
  .data$n_col <- col_n[col_id]
  .data$n_expected <- expected[index]
  .data$chi2_contrib <- contributions[index]
  .data$chi2_pearson <- rep(sum(contributions), nrow(.data))
  .data$r <- rep(nr, nrow(.data))
  .data$c <- rep(nc, nrow(.data))
  .data$df <- rep((nr - 1L) * (nc - 1L), nrow(.data))
  .data$p_chi2_pearson <- stats::pchisq(.data$chi2_pearson, .data$df,
                                      lower.tail = FALSE)
  .data$p_fisher <- NULL
  if (min(expected) <= 5) {
    message("One or more expected cell counts are <= 5. ",
            "Fisher's exact p-value is also returned.")
    .data$p_fisher <- rep(stats::fisher.test(counts)$p.value, nrow(.data))
  }
  .data
}

#' @rdname freq_test
#' @export
freq_test.default <- function(.data, ...) {
  stop("freq_test() supports freq_table() results with zero or one grouping ",
       "variable. For multiple groups, select an explicitly defined analysis.",
       call. = FALSE)
}

check_test_input <- function(data, dots) {
  if (length(dots)) stop("freq_test() does not support extra arguments in ....",
                        call. = FALSE)
  meta <- attr(data, "freqtables", exact = TRUE)
  if (is.null(meta)) {
    stop("Recreate the table with freq_table() to retain analysis metadata.",
         call. = FALSE)
  }
  columns <- if (meta$generic_col_names) {
    c(sprintf("group_%02d_cat", seq_along(meta$groups)), "cat")
  } else c(meta$groups, meta$outcome)
  generated <- c("n_expected", "chi2_contrib", "chi2_pearson", "df",
                 "p_chi2_pearson", "n_col", "r", "c", "p_fisher")
  if (length(intersect(columns, generated))) {
    stop("Rename analysis columns that conflict with test result names: ",
         paste(intersect(columns, generated), collapse = ", "), ".",
         call. = FALSE)
  }
  if (!all(c(columns, "n") %in% names(data)) ||
      !is.numeric(data$n) || any(!is.finite(data$n) | data$n < 0 |
                                 data$n != floor(data$n)) ||
      sum(data$n) != meta$n_total ||
      anyDuplicated(as.data.frame(data[columns]))) {
    stop("freq_test() requires a complete table with intact category/count ",
         "columns; recreate it from the desired input observations.",
         call. = FALSE)
  }
  if (meta$generic_col_names) {
    variable_columns <- c(sprintf("group_%02d_col", seq_along(meta$groups)), "col")
    values <- c(meta$groups, meta$outcome)
    for (i in seq_along(values)) {
      if (!variable_columns[i] %in% names(data) ||
          anyNA(data[[variable_columns[i]]]) ||
          any(data[[variable_columns[i]]] != values[i])) {
        stop("Do not stack different analyses before freq_test().", call. = FALSE)
      }
    }
  }
  meta
}
