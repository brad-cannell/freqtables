# =============================================================================
# Study data
# This is the code to create the study data - a simulated data set that can be
# used to demonstrate how to use the freqtables package.
# Created: 2022-07-24
# Brad Cannell
# =============================================================================

library(dplyr, warn.conflicts = FALSE)

set.seed(123)

study <- tibble(
  id        = factor(c(1:100)),
  age       = c(32, 30, 32, 29, 24, 38, 25, 24, 48, 29, 22, 29, 24, 28, 24, 25,
                25, 22, 25, 24, 25, 24, 23, 24, 31, 24, 29, 24, 22, 23, 26, 23,
                24, 25, 24, 33, 27, 25, 26, 26, 26, 26, 26, 27, 24, 43, 25, 24,
                27, 28, 29, 24, 26, 28, 25, 24, 26, 24, 26, 31, 24, 26, 31, 34,
                26, 25, 27, 40, 35, 43, 81, 18, 79, 45, 37, 66, 35, 81, 50, 54,
                24, 47, 84, 38, 23, 74, 77, 65, 46, 55, 41, 84, 41, 29, 60, 60,
                40, 53, 31, 77),
  age_group = c(2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2,
                2, 1, 1, 1, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2,
                1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2),
  sex       = c(2, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1,
                1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1,
                1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1,
                1, 1, 2, 1, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 2, 2,
                1, 2, 1, 2, 2, 2, 1, 2, 2, 1, 1, 2, 2, 2, 1, 1),
  ht_in     = c(70, 63, 62, 67, 67, 58, 64, 69, 65, 68, 63, 68, 69, 66, 67, 65,
                64, 75, 67, 63, 60, 67, 64, 73, 62, 69, 67, 62, 68, 66, 66, 62,
                64, 68, NA, 68, 70, 68, 68, 66, 71, 61, 62, 64, 64, 63, 67, 66,
                69, 76, NA, 63, 64, 65, 65, 71, 66, 65, 65, 71, 64, 71, 60, 62,
                61, 69, 66, NA, 73, 71, 72, 76, 74, 63, 65, 65, 73, 76, 66, 58,
                65, 65, 65, 68, 71, 68, 60, 70, 68, 62, 74, 68, 76, 72, 59, 76,
                61, 72, 69, 59),
  wt_lbs    = c(216, 106, 145, 195, 143, 125, 138, 140, 158, 167, 145, 297, 146,
                125, 111, 125, 130, 182, 170, 121, 98, 150, 132, 250, 137, 124,
                186, 148, 134, 155, 122, 142, 110, 132, 188, 176, 188, 166, 136,
                147, 178, 125, 102, 140, 139, 60, 147, 147, 141, 232, 186, 212,
                110, 110, 115, 154, 140, 150, 130, NA, 171, 156, 92, 122, 102,
                163, 141, NA, 106, 118, 205, 229, 198, 255, 268, 203, 269, 162,
                115, 106, 139, 246, 147, 206, 202, 209, 123, 265, 119, 171, 195,
                136, 208, 108, 201, 224, 261, 169, 295, 93)
)

# Add missing values for testing
study$age_group[3] <- NA
study$sex[4] <- NA

study <- study %>%
  # Add calculated variables
  mutate(
    bmi      = round(wt_lbs / ht_in^2 * 703, 2),
    bmi_4cat = case_when(
      is.na(bmi) ~ NA_real_, # Missing
      bmi < 18.5 ~ 1, # Underweight
      bmi < 25   ~ 2, # Normal weight
      bmi < 30   ~ 3, # Overweight
      bmi < 50   ~ 4, # Obese
      TRUE       ~ 5  # Extreme Obese. I don't actually expect anyone in this
      # category. I'm creating it to test the `drop` argument.
    )
  ) %>%
  # Create exposure and outcome where,
  # Exposure is more likely in people over age 30 and
  # Outcome is more likely in people with exposure
  mutate(
    exposure = if_else(
      age_group == 1,
      sample(c(0,1), 100, TRUE, c(.9, .1)),
      sample(c(0,1), 100, TRUE, c(.7, .3))
    ),
    outcome = if_else(
      exposure == 1,
      sample(c(0,1), 100, TRUE, c(.8, .2)),
      sample(c(0,1), 100, TRUE, c(.5, .5))
    )
  ) %>%
  # Make factors
  mutate(
    age_group = factor(age_group, labels = c("Younger than 30", "30 and Older")),
    sex       = factor(sex, labels = c("Female", "Male")),
    bmi_4cat  = factor(bmi_4cat, 1:5, labels = c("Underweight", "Normal", "Overweight", "Obese", "Extreme Obese")),
    exposure  = factor(exposure, labels = c("No", "Yes")),
    outcome   = factor(outcome, labels = c("No", "Yes"))
  )

# Because this is for freqtables, keep the categorical variables only
freq_study <- study %>%
  select(id, age_group, sex, bmi_4cat, exposure, outcome)


# Add the simulated data to the data directory.
usethis::use_data(freq_study, overwrite = TRUE)

# Export the data into file formats for other software packages that we want
# to use in examples.
# We use the Stata data for one of the examples in README.
readr::write_csv(freq_study, "inst/extdata/freq_study.csv")
haven::write_dta(freq_study, "inst/extdata/freq_study.dta")
haven::write_xpt(freq_study, "inst/extdata/freq_study.xpt")
