small_data <- function() {
  data.frame(
    group = rep(c("A", "A", "B", "B"), c(3, 7, 4, 2)),
    outcome = rep(c("no", "yes", "no", "yes"), c(3, 7, 4, 2))
  )
}
