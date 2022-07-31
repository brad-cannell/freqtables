# Start from scratch. Begin with a shortened version
# (freq_tbl) that only returns column names, n, and percent. The other
# calculations will be rolled into their own functions too.
#
# Delete all of this and add Roxygen comments when the code is working

# This function can be used independently. It is also used as the first function in in freq_table.

# Adding the ability to turn off percent calculation because it will save
# computation when the result is passed to freq_table(). However, percent = TRUE
# will be the default because that's probably how much people will want the
# results displayed when using freq_tbl independent of freq_table().

freq_tbl <- function(.data, .freq_var, .drop = FALSE, percent = TRUE) {

  # Get n overall
  n_overall <- nrow(.data)
  # Get counts
  out <- .data %>%
    dplyr::count({{ .freq_var }}, .drop = .drop) %>%
    dplyr::mutate(
      # Both of these values appear in one-way and n-way tables
      n_overall    = n_overall,
      prop_overall = n / n_overall
    )

  # Convert overall proportion to percentage (optional)
  if (percent) {
    out <- out %>%
      dplyr::mutate(
        percent_overall = prop_overall * 100,
        prop_overall    = NULL
      )
  }

  # grouped add n_group and percent_group
  if ("grouped_df" %in% class(.data)) {
    out <- out %>%
      dplyr::mutate(
        n_group    = sum(n),
        prop_group = n / sum(n),
      )

    # Convert group proportion to percentage (optional)
    # Grouped tibbles only
    if (percent) {
      out <- out %>%
        dplyr::mutate(
          percent_group = prop_group * 100,
          prop_group    = NULL
        )
    }
  }

  # Return tibble of results
  out
}

# For testing
# data(freq_study)
# library(dplyr)
# devtools::load_all()
# freq_study %>% freq_tbl() # Return informative error
# freq_study %>% freq_tbl(sex)
# freq_study %>% group_by(exposure) %>% freq_tbl(sex)
# freq_study %>% group_by(exposure) %>% freq_tbl(sex, percent = FALSE)
# freq_study %>% group_by(exposure, outcome) %>% freq_tbl(sex, .drop = FALSE)
# freq_table(freq_study, exposure, sex)
