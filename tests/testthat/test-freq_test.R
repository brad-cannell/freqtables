test_that("one-way tests retain the existing statistical results", {
  out <- freq_table(mtcars, am) |> freq_test()
  reference <- chisq.test(c(19, 13))
  expect_equal(out$n_expected, c(16, 16))
  expect_equal(out$chi2_contrib, c(.5625, .5625))
  expect_equal(out$chi2_pearson, rep(unname(reference$statistic), 2))
  expect_equal(out$p_chi2_pearson, rep(reference$p.value, 2))
  expect_equal(out$df, c(1, 1))
})

test_that("rectangular tables use category identities in any row order", {
  for (generic in c(FALSE, TRUE)) {
    for (flip in c(FALSE, TRUE)) {
      x <- if (flip) mtcars |> dplyr::group_by(cyl) |>
        freq_table(am, generic_col_names = generic) else
        mtcars |> dplyr::group_by(am) |>
        freq_table(cyl, generic_col_names = generic)
      x <- x[c(6, 2, 4, 1, 3, 5), ]
      expect_message(out <- freq_test(x), "Fisher")
      reference <- table(mtcars$am, mtcars$cyl)
      expect_equal(out$p_chi2_pearson,
                   rep(suppressWarnings(chisq.test(reference, correct = FALSE))$p.value, 6))
      expect_equal(out$p_fisher, rep(fisher.test(reference)$p.value, 6))
    }
  }
})

test_that("implicit zero cells and high expected counts use correct tests", {
  d <- data.frame(g = rep(c("a", "b", "b"), c(10, 10, 10)),
                   y = rep(c("no", "no", "yes"), c(10, 10, 10)))
  x <- d |> dplyr::group_by(g) |> freq_table(y, drop = TRUE)
  expect_message(out <- freq_test(x), "Fisher")
  ref <- table(d$g, d$y)
  expect_equal(out$p_fisher, rep(fisher.test(ref)$p.value, 3))
  expect_equal(out$chi2_pearson,
               rep(unname(suppressWarnings(chisq.test(ref, correct = FALSE))$statistic), 3))
  d <- small_data()[rep(seq_len(16), 10), ]
  out <- d |> dplyr::group_by(group) |> freq_table(outcome) |> freq_test()
  expect_false("p_fisher" %in% names(out))
  expect_equal(out$p_chi2_pearson,
               rep(chisq.test(table(d$group, d$outcome), correct = FALSE)$p.value, 4))
})

test_that("unsupported tests and incomplete tables fail clearly", {
  expect_error(freq_table(data.frame(df = c("a", "b")), df) |> freq_test(),
               "conflict with test result")
  expect_error(freq_test(freq_tbl(mtcars, am)), "zero or one")
  expect_error(mtcars |> dplyr::group_by(cyl, vs) |> freq_table(am) |> freq_test(),
               "zero or one")
  expect_error(freq_table(mtcars, am) |> freq_test(method = "fisher"), "extra arguments")
  expect_error(freq_test(freq_table(mtcars, am)[1, ]), "complete table")
  x <- freq_table(mtcars, am)
  expect_error(freq_test(x[c(1, 1), ]), "complete table")
  d <- data.frame(g = factor(c("a", "b"), levels = c("a", "b", "c")), y = c("x", "y"))
  expect_error(d |> dplyr::group_by(g) |> freq_table(y) |> freq_test(), "marginal")
  expect_error(freq_table(data.frame(y = "only"), y) |> freq_test(), "two categories")
})
