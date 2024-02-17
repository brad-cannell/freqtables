#' Simulated study data.
#'
#' This is the code to create the study data - a simulated datas et that can be
#' used to demonstrate how to use the `freqtables` package.
#'
#' @format A data frame with 100 rows and 6 variables:
#' \describe{
#'   \item{id}{Participant's study identification number}
#'   \item{age_group}{Participant's age at the time of enrollment categorized as "Younger than 30" or "30 and Older"}
#'   \item{sex}{Biological sex of the participant assigned at birth, female/male}
#'   \item{bmi_4cat}{Participant's bmi at date of enrollment categorized as "Underweight", "Normal", "Overweight", or "Obese"}
#'   \item{exposure}{Participant was exposed to a hypothetical risk factor of interest, Yes/No}
#'   \item{outcome}{Participant experienced a hypothetical outcome of interest, Yes/No}
#' }
"freq_study"
