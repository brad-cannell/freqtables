test_that("helpers match independent ungrouped t calculations and legacy values", {
  p <- 19 / 32
  se <- sqrt(p * (1 - p) / 31)
  t <- qt(.975, 31)
  x <- freq_tbl(mtcars, am)
  w <- wald_ci(x, se = TRUE, critical_value = TRUE)
  l <- logit_ci(x)
  expect_equal(w$se[1], se)
  expect_equal(w$t_crit[1], t)
  expect_equal(w$lcl[1], p - t * se)
  expect_equal(w$ucl[1], p + t * se)
  expect_equal(l$lcl[1], plogis(qlogis(p) - t * se / (p * (1 - p))))
  expect_equal(round(l$lcl * 100, 2), c(40.94, 24.50))
  expect_equal(round(l$ucl * 100, 2), c(75.50, 59.06))
  expect_equal(round(w$lcl * 100, 2), c(41.38, 22.63))
  expect_equal(logit_ci(x, percent_ci = 99)$lcl[1],
               plogis(qlogis(p) - qt(.995, 31) * se / (p * (1 - p))))
})

test_that("each group's denominator determines both SE and t critical values", {
  x <- small_data() |> dplyr::group_by(group) |>
    freq_tbl(outcome, overall = TRUE)
  for (helper in list(wald_ci, logit_ci)) {
    ci <- helper(x, se = TRUE, critical_value = TRUE)
    expect_equal(ci$se_group, sqrt(c(.3 * .7 / 9, .7 * .3 / 9,
                                     (4/6) * (2/6) / 5, (2/6) * (4/6) / 5)))
    expect_equal(ci$t_crit_group, qt(.975, c(9, 9, 5, 5)))
    expect_equal(ci$t_crit_total, rep(qt(.975, 15), 4))
  }
  expected_lower <- plogis(qlogis(.3) - qt(.975, 9) *
                            sqrt(.3 * .7 / 9) / (.3 * .7))
  expect_equal(logit_ci(x)$lcl_group[1], expected_lower)
  expect_false(isTRUE(all.equal(logit_ci(x)$lcl_group[1],
    plogis(qlogis(.3) - qt(.975, 15) * sqrt(.3 * .7 / 9) / (.3 * .7)))))
})

test_that("percentage scale and repeated calls do not corrupt intervals", {
  d <- small_data() |> dplyr::group_by(group)
  for (helper in list(wald_ci, logit_ci)) {
    x <- helper(freq_tbl(d, outcome, overall = TRUE),
                 se = TRUE, critical_value = TRUE)
    y <- helper(freq_tbl(d, outcome, overall = TRUE, percent = TRUE),
                 se = TRUE, critical_value = TRUE)
    for (nm in c("lcl_group", "ucl_group", "lcl_total", "ucl_total",
                 "se_group", "se_total")) expect_equal(y[[nm]], x[[nm]] * 100)
    expect_equal(y$t_crit_group, x$t_crit_group)
    expect_equal(helper(y), helper(freq_tbl(d, outcome,
                                            overall = TRUE, percent = TRUE)))
    expect_false(any(grepl("^(se|t_crit)", names(helper(x)))))
  }
})

test_that("denominators survive filtering and generic conversion", {
  x <- freq_tbl(mtcars, am)
  filtered <- dplyr::filter(x, am == 0)
  expect_equal(logit_ci(filtered)$lcl, logit_ci(x)$lcl[1])
  expect_equal(logit_ci(freq_tbl(mtcars, am, generic_col_names = TRUE))$lcl,
               logit_ci(x)$lcl)
  plain <- data.frame(n = 19, n_total = 32, prop = 19/32)
  expect_equal(wald_ci(plain)$lcl, wald_ci(x)$lcl[1])
  expect_error(wald_ci(data.frame(n = 19, prop = 19/32)), "Missing n_total")
})

test_that("boundaries, empty tables and invalid inputs are explicit", {
  d <- data.frame(y = factor(rep("yes", 4), levels = c("no", "yes")))
  x <- freq_tbl(d, y)
  expect_true(all(is.na(logit_ci(x)$lcl)))
  expect_equal(wald_ci(x)$lcl, c(0, 1))
  for (n in c(0, 1)) {
    z <- freq_tbl(d[seq_len(n), , drop = FALSE], y)
    ci <- wald_ci(z, se = TRUE, critical_value = TRUE)
    expect_true(all(is.na(ci$lcl)))
    expect_true(all(is.na(ci$se)))
    expect_true(all(is.na(ci$t_crit)))
  }
  expect_equal(nrow(logit_ci(freq_tbl(data.frame(y = character()), y))), 0)
  extreme <- data.frame(n = c(1, 999999), n_total = 1000000,
                         prop = c(.000001, .999999))
  expect_true(all(is.finite(logit_ci(extreme)$lcl)))
  expect_lt(wald_ci(data.frame(n = 1, n_total = 3, prop = 1/3))$lcl, 0)
  for (bad in list(0, 100, NA_real_, Inf, "95", c(90, 95))) {
    expect_error(wald_ci(x, percent_ci = bad), "strictly between")
  }
  expect_error(wald_ci(x, se = 1), "TRUE or FALSE")
  expect_error(wald_ci(data.frame(n = -1, prop = .2, n_total = 5)), "counts")
  expect_error(wald_ci(data.frame(n = 1, prop = .2, n_total = 0)), "denominators")
  expect_error(wald_ci(data.frame(n = 1, prop = .3, n_total = 5)), "does not agree")
  expect_error(wald_ci(data.frame(n = 1, prop = .2, percent = 20, n_total = 5)),
               "not both")
})
