test_that("table output is exactly counts plus the selected interval helper", {
  for (groups in list(character(), "cyl", c("cyl", "vs"))) {
    d <- if (length(groups)) dplyr::group_by(mtcars, !!!rlang::syms(groups)) else mtcars
    for (method in c("wald", "logit")) for (percent in c(FALSE, TRUE)) {
      x <- freq_table(d, am, ci_type = method, percent_ci = 90,
                      percent = percent, overall = TRUE, se = TRUE,
                      critical_value = TRUE, generic_col_names = TRUE)
      helper <- if (method == "wald") wald_ci else logit_ci
      y <- helper(freq_tbl(d, am, percent = percent, overall = TRUE,
                            generic_col_names = TRUE),
                   percent_ci = 90, se = TRUE, critical_value = TRUE)
      expect_equal(as.data.frame(x), as.data.frame(y))
      expect_identical(attr(x, "freqtables"), attr(y, "freqtables"))
      expect_false(dplyr::is_grouped_df(x))
    }
  }
})

test_that("compact table schemas and classes are stable", {
  x <- freq_table(mtcars, am)
  expect_identical(names(x), c("am", "n", "prop", "lcl", "ucl"))
  expect_s3_class(x, "freq_table_one_way")
  y <- mtcars |> dplyr::group_by(cyl) |> freq_table(am)
  expect_identical(names(y), c("cyl", "am", "n", "n_group", "prop_group",
                               "lcl_group", "ucl_group"))
  expect_s3_class(y, "freq_table_two_way")
  z <- mtcars |> dplyr::group_by(cyl, vs) |> freq_table(am)
  expect_s3_class(z, "freq_table_n_way")
  d <- data.frame(y = factor(c("a", "a", "b"), levels = c("a", "b", "c")))
  expect_equal(nrow(freq_table(d, y, drop = FALSE)), 3)
  expect_equal(nrow(freq_table(d, y, drop = TRUE)), 2)
})

test_that("migration and invalid option errors are informative", {
  expect_error(freq_table(mtcars, cyl, am), "group_by")
  expect_error(freq_table(mtcars, am, 99), "named options")
  expect_error(freq_table(mtcars), "requires one column")
  expect_error(freq_table(mtcars, am, ci_type = "log"), 'logit.*wald')
  expect_error(freq_table(mtcars, am, ci_type = NA), 'logit.*wald')
  expect_error(freq_table(mtcars, am, percent_ci = 100), "strictly between")
  expect_error(freq_table(mtcars, am, critical_value = NA), "TRUE or FALSE")
})
