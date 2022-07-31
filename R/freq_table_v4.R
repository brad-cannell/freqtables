# Start from scratch. Begin with a shortened version
# (freq_tbl) that only returns column names, n, and percent. The other
# calculations will be rolled into their own functions too.
#
# Delete all of this and add Roxygen comments when the code is working

# freq_tbl function can be used independently. It is also used as the first function in in freq_table.

freq_table_v4 <- function(.data, .freq_var, percent_ci = 95, ci_type = "logit", .drop = FALSE) {

  # Convert percent_95 to t_prob
  alpha <-  1 - (percent_ci / 100)
  t_prob <- 1 - alpha / 2

  # Get freq_tbl
  freq_tbl <- .data %>% freq_tbl({{ .freq_var }}, .drop = .drop, percent = FALSE)

  # Update out to include elements needed for Wald and Logit transformed CI's
  # This can be used for one-way and n-way tables both.
  out <- freq_tbl %>%
    dplyr::mutate(
      se_overall      = sqrt(prop_overall * (1 - prop_overall) / (n_overall - 1)),
      t_crit_overall  = stats::qt(t_prob, df = n_overall - 1)
    )

  # ===========================================================================
  # Add confidence interval stuff next. Turn into functions?
  # ===========================================================================

  # Calculate Wald CI's
  # -------------------
  # and put prop, se, and CI's on percent scale
  # One-way tables

  # Turn into function
  out <- out %>%
    dplyr::mutate(
      lcl_wald   = prop_overall - t_crit_overall * se_overall,
      ucl_wald   = prop_overall + t_crit_overall * se_overall,
      percent    = prop_overall * 100,
      se_overall = se_overall * 100,
      lcl        = lcl_wald * 100,
      ucl        = ucl_wald * 100
    )

  # Return tibble of results
  out
}



# For testing
# devtools::load_all()
# data(freq_study)
# freq_study %>% freq_tbl(sex, percent = FALSE)
# freq_study %>% freq_table_v4()
# freq_study %>% freq_table_v4(sex)
# freq_study %>% freq_table(sex, ci_type = "wald") # For comparison
# freq_study %>% group_by(exposure) %>% freq_table_v4(sex)
# freq_study %>% freq_table(exposure, sex) # For comparison
# freq_study %>% group_by(exposure) %>% freq_table_v4(sex, outcome)
# freq_study %>% group_by(exposure) %>% freq_table_v4(sex, "wald")
