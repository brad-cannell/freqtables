test_that("legacy percent strings and integer formatting remain stable", {
  x <- freq_table(mtcars, am, percent = TRUE) |>
    freq_format("percent (lcl - ucl)", name = "percent_ci", digits = 2)
  expect_identical(x$percent_ci,
                   c("59.38 (40.94 - 75.50)", "40.62 (24.50 - 59.06)"))
  y <- freq_table(mtcars, am, percent = TRUE) |> freq_format("n (percent%)", digits = 2)
  expect_identical(y$formatted_stats, c("19 (59.38%)", "13 (40.62%)"))
  expect_equal(x$percent, c(19, 13) / 32 * 100)
})

test_that("grouped, literal-prefix, category, and empty formatting works", {
  x <- small_data() |> dplyr::group_by(group) |> freq_table(outcome) |>
    freq_format("[group] n: prop_group", digits = 2)
  expect_identical(x$formatted_stats,
                   c("[A] 3: 0.30", "[A] 7: 0.70", "[B] 4: 0.67", "[B] 2: 0.33"))
  expect_identical(freq_format(data.frame(n = 1L), "n")$formatted_stats, "1")
  expect_identical(freq_format(data.frame(n = integer()), "n")$formatted_stats,
                   character())
  expect_identical(freq_format(data.frame(n = 1L), "n", name = "n")$n, "1")
  expect_error(freq_format(data.frame(n = 1L), "n", digits = -1), "nonnegative")
  expect_error(freq_format(data.frame(n = 1L), NA_character_), "string")
})
