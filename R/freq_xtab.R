#' Contingency count matrices and grouped arrays
#'
#' Cross-tabulate two columns, with exposure in rows and outcome in columns.
#' Groups supplied by [dplyr::group_by()] become additional array dimensions
#' in the supplied order. Every input row contributes once, including missing
#' values; filter observations explicitly before counting to exclude them.
#'
#' @param .data A local data frame or grouped data frame. Rowwise data is not
#'   supported; call [dplyr::ungroup()] first.
#' @param exposure,outcome Two distinct unquoted column names, or strings naming
#'   columns. Forward columns in wrappers with `{{ }}`. Neither column may also
#'   be a grouping variable. Columns must be one-dimensional atomic vectors
#'   (for example factors, character, numeric, logical, or Date vectors).
#' @param ... Must be empty. Options must be named; derived columns should be
#'   created with [dplyr::mutate()] before calling.
#' @param margins Logical. If `TRUE` (default), append a total row and column
#'   to each matrix, labeled `Sum`. If that label already exists on an axis,
#'   use the first available `Sum.1`, `Sum.2`, etc. Group dimensions have no
#'   margins. Use `FALSE` to obtain only the cell counts for further analysis.
#' @param drop Logical. If `FALSE` (default), retain unused factor levels on
#'   every axis. If `TRUE`, remove globally unused levels, overriding the
#'   input's grouping drop setting. Both settings fill absent combinations
#'   with zero so every group has the same matrix dimensions. Missing values
#'   are retained independently of this option.
#'
#' @return A numeric matrix for ungrouped input, or a numeric array with
#'   dimensions exposure, outcome, then each grouping variable. Dimension names
#'   are the original column names. Factor categories follow level order;
#'   other categories are sorted (character values in C-locale order), with
#'   missing values last. Labels are character representations of categories;
#'   ambiguous representations of distinct values are rejected. Missing labels
#'   are actual `NA_character_`, distinct from literal strings such as `"NA"`.
#'   Select a missing category by position using `which(is.na(dimnames(x)[[i]]))`.
#'
#'   Unobserved combinations, including empty group combinations, have zero
#'   counts. Empty input retains factor levels when `drop = FALSE`; other axes
#'   have length zero before margins. Thus empty ungrouped character input is
#'   a 1-by-1 zero matrix with margins, or a 0-by-0 matrix without. A grouped
#'   result with a zero-length grouping axis has no matrix slices. Singleton
#'   dimensions are retained in the returned object; use `drop = FALSE` when
#'   subsetting if those dimensions should remain.
#'
#' @export
#' @examples
#' freq_xtab(mtcars, vs, am)
#' freq_xtab(mtcars, "vs", "am", margins = FALSE)
#' mtcars |> dplyr::group_by(cyl) |> freq_xtab(vs, am)
#' mtcars |> dplyr::group_by(cyl, gear) |> freq_xtab(vs, am, drop = TRUE)
freq_xtab <- function(.data, exposure, outcome, ..., margins = TRUE,
                      drop = FALSE) {
  variables <- check_xtab_input(.data, rlang::enquo(exposure),
                                rlang::enquo(outcome), rlang::enquos(...))
  check_flag(margins, "margins")
  check_flag(drop, "drop")
  values <- lapply(variables, function(column) {
    value <- .data[[column]]
    if (!is.atomic(value) || !is.null(dim(value)) ||
        is.complex(value) || is.raw(value)) {
      stop("Column '", column, "' must be a one-dimensional categorical vector.",
           call. = FALSE)
    }
    if (is.factor(value)) return(value)
    # Count NaN and NA as the same missing category.
    value[is.na(value)] <- NA
    value
  })
  categories <- lapply(values, xtab_categories, drop = drop)
  labels <- lapply(categories, as.character)
  for (i in seq_along(labels)) {
    if (anyDuplicated(labels[[i]])) {
      stop("Column '", variables[i], "' has distinct categories with identical ",
           "text labels; recode them with unique labels first.", call. = FALSE)
    }
  }
  dimensions <- lengths(categories)
  if (margins) {
    dimensions[1:2] <- dimensions[1:2] + 1L
    for (i in 1:2) {
      label <- "Sum"
      suffix <- 0L
      while (label %in% labels[[i]]) {
        suffix <- suffix + 1L
        label <- paste0("Sum.", suffix)
      }
      labels[[i]] <- c(labels[[i]], label)
    }
  }
  names(labels) <- variables
  size <- prod(as.double(dimensions))
  if (!is.finite(size) || size > .Machine$integer.max) {
    stop("The requested dense array is too large; reduce category levels or ",
         "grouping variables.", call. = FALSE)
  }
  ids <- Map(function(value, category) {
    if (is.factor(value)) value <- as.character(value)
    match(value, category)
  }, values, categories)
  strides <- cumprod(c(1, utils::head(as.double(dimensions), -1L)))
  index <- rep(1, nrow(.data))
  for (i in seq_along(ids)) index <- index + (ids[[i]] - 1) * strides[i]
  counts <- as.double(tabulate(index, nbins = size))
  if (margins && nrow(.data)) {
    # Accumulate each observation at its two margins and grand total as well
    # as its cell. This preserves empty and singleton dimensions without apply.
    to_row_total <- dimensions[1] - ids[[1]]
    to_col_total <- (dimensions[2] - ids[[2]]) * dimensions[1]
    counts <- counts + tabulate(index + to_row_total, nbins = size)
    counts <- counts + tabulate(index + to_col_total, nbins = size)
    counts <- counts + tabulate(index + to_row_total + to_col_total, nbins = size)
  }
  array(counts, dim = dimensions, dimnames = labels)
}

xtab_categories <- function(value, drop) {
  if (is.factor(value)) {
    categories <- levels(value)
    if (drop) categories <- categories[categories %in% as.character(value)]
    categories <- categories[!is.na(categories)]
    if (anyNA(as.character(value)) || (!drop && anyNA(levels(value)))) {
      categories <- c(categories, NA_character_)
    }
    return(categories)
  }
  categories <- unique(value[!is.na(value)])
  categories <- if (is.character(categories)) {
    sort(categories, method = "radix")
  } else sort(categories)
  if (anyNA(value)) categories <- c(categories, value[NA_integer_])
  categories
}

check_xtab_input <- function(data, exposure, outcome, dots) {
  if (!is.data.frame(data)) {
    stop("freq_xtab() expects .data to be a local data frame.", call. = FALSE)
  }
  if (inherits(data, "rowwise_df")) {
    stop("Rowwise data is not supported; call dplyr::ungroup() first.",
         call. = FALSE)
  }
  if (anyDuplicated(names(data)) || anyNA(names(data)) || any(names(data) == "")) {
    stop(".data must have unique, nonempty column names.", call. = FALSE)
  }
  if (length(dots)) {
    stop("freq_xtab() accepts two columns and named options only; ... must ",
         "be empty. Supply groups with dplyr::group_by().", call. = FALSE)
  }
  columns <- vapply(list(exposure = exposure, outcome = outcome), function(var) {
    if (rlang::quo_is_missing(var)) {
      stop("freq_xtab() requires both exposure and outcome columns.", call. = FALSE)
    }
    expr <- rlang::quo_get_expr(var)
    if (!(rlang::is_symbol(expr) || rlang::is_string(expr))) {
      stop("exposure and outcome must name existing columns; create derived ",
           "columns with dplyr::mutate() first.", call. = FALSE)
    }
    column <- rlang::as_name(var)
    if (!column %in% names(data)) {
      stop("Column '", column, "' is not in .data.", call. = FALSE)
    }
    column
  }, character(1))
  if (columns[1] == columns[2]) {
    stop("exposure and outcome must be distinct columns.", call. = FALSE)
  }
  groups <- dplyr::group_vars(data)
  if (any(columns %in% groups)) {
    stop("exposure and outcome must not also be grouping variables.", call. = FALSE)
  }
  c(unname(columns), groups)
}
