xtab_fixture <- function(counts = c(3, 2, 3, 2)) {
  data.frame(
    exposure = factor(rep(c("Yes", "Yes", "No", "No"), counts),
                      levels = c("Yes", "No")),
    outcome = factor(rep(c("Yes", "No", "Yes", "No"), counts),
                     levels = c("Yes", "No"))
  )
}

test_that("source fixture has the specified orientation, counts, and margins", {
  d <- xtab_fixture()
  expected <- matrix(c(3, 2, 5, 3, 2, 5, 6, 4, 10), 3, byrow = TRUE,
    dimnames = list(exposure = c("Yes", "No", "Sum"),
                    outcome = c("Yes", "No", "Sum")))
  x <- freq_xtab(d, exposure, outcome)
  expect_identical(x, expected)
  expect_identical(class(x), c("matrix", "array"))
  expect_identical(freq_xtab(d, exposure, outcome, margins = FALSE),
                   expected[1:2, 1:2])
  expect_identical(freq_xtab(d, "exposure", "outcome"), x)
  wrapper <- function(data, e, o) freq_xtab(data, {{ e }}, {{ o }})
  expect_identical(wrapper(d, exposure, outcome), x)
  expect_identical(freq_xtab(d, outcome = outcome, exposure = exposure), x)
})

test_that("asymmetric nonbinary counts are independent of input row order", {
  d <- data.frame(
    exposure = factor(rep(rep(c("z", "a"), each = 3), 1:6), c("z", "a")),
    outcome = factor(rep(rep(c("third", "first", "second"), 2), 1:6),
                     c("third", "first", "second")))
  cells <- matrix(as.double(1:6), 2, byrow = TRUE,
    dimnames = list(exposure = c("z", "a"),
                    outcome = c("third", "first", "second")))
  x <- freq_xtab(d, exposure, outcome)
  expect_identical(x[1:2, 1:3], cells)
  expect_equal(unname(x[1:2, 4]), c(6, 15))
  expect_equal(unname(x[3, 1:3]), c(5, 7, 9))
  expect_equal(x[3, 4], 21)
  expect_equal(x, unclass(addmargins(table(d))))
  expect_identical(freq_xtab(d[rev(seq_len(nrow(d))), ], exposure, outcome), x)
})

test_that("group arrays retain axes and zero-fill globally defined categories", {
  d <- rbind(transform(xtab_fixture(c(1, 2, 0, 0)), site = "A", sex = "F"),
             transform(xtab_fixture(c(0, 0, 3, 4)), site = "A", sex = "M"),
             transform(xtab_fixture(c(5, 0, 0, 6)), site = "B", sex = "M"))
  grouped <- d |> dplyr::group_by(site, sex)
  x <- freq_xtab(grouped, exposure, outcome)
  expect_identical(dim(x), c(3L, 3L, 2L, 2L))
  expect_identical(names(dimnames(x)), c("exposure", "outcome", "site", "sex"))
  expect_equal(x["Sum", "Sum", , ], matrix(c(3, 0, 7, 11), 2,
    dimnames = list(site = c("A", "B"), sex = c("F", "M"))))
  expect_true(all(x[, , "B", "F"] == 0))
  expect_equal(unname(x[1:2, 1:2, "A", "M"]), matrix(c(0, 0, 3, 4), 2, byrow = TRUE))
  ref <- table(d$exposure, d$outcome, d$site, d$sex,
               dnn = c("exposure", "outcome", "site", "sex"))
  expect_equal(x, unclass(addmargins(ref, margin = 1:2)))
  expect_equal(freq_xtab(grouped, exposure, outcome, margins = FALSE), unclass(ref))
  swapped <- d |> dplyr::group_by(sex, site) |> freq_xtab(exposure, outcome)
  expect_identical(swapped, aperm(x, c(1, 2, 4, 3)))
  single <- d |> dplyr::group_by(site) |> freq_xtab(exposure, outcome)
  expect_identical(dim(single), c(3L, 3L, 2L))
  expect_equal(single[, , "A"], freq_xtab(d[d$site == "A", ], exposure, outcome))
})

test_that("drop prunes unused factors globally and overrides input grouping drop", {
  d <- data.frame(e = factor(c("yes", "no"), c("yes", "no", "unused")),
                  o = factor(c("yes", "no"), c("yes", "no", "unused")),
                  g = factor(c("A", "B"), c("B", "A", "empty")))
  kept <- d |> dplyr::group_by(g) |> freq_xtab(e, o)
  expect_identical(dim(kept), c(4L, 4L, 3L))
  expect_identical(dimnames(kept)$g, c("B", "A", "empty"))
  expect_true(all(kept[, , "empty"] == 0))
  dropped <- d |> dplyr::group_by(g, .drop = FALSE) |> freq_xtab(e, o, drop = TRUE)
  expect_identical(dim(dropped), c(3L, 3L, 2L))
  expect_equal(unname(dropped[1:2, 1:2, "A"]), matrix(c(1, 0, 0, 0), 2))
  expect_equal(unname(dropped[1:2, 1:2, "B"]), matrix(c(0, 0, 0, 1), 2))
  expect_identical(dropped, kept[c(1, 2, 4), c(1, 2, 4), 1:2, drop = FALSE])
})

test_that("three grouping axes and missing values match independent base counts", {
  grid <- expand.grid(e = c(-2, 20, NA), o = c("a", "z", NA),
                       g1 = c(1, 2, NA), g2 = c(FALSE, TRUE), g3 = c("a", "b"),
                       stringsAsFactors = FALSE)
  counts <- seq_len(nrow(grid)) %% 5
  d <- grid[rep(seq_len(nrow(grid)), counts), ]
  d <- d[rev(seq_len(nrow(d))), ]
  x <- d |> dplyr::group_by(g1, g2, g3) |> freq_xtab(e, o)
  reference <- table(d, useNA = "ifany")
  expect_identical(dim(x), c(4L, 4L, 3L, 2L, 2L))
  expect_equal(x, unclass(addmargins(reference, margin = 1:2)))
  expect_equal(sum(x["Sum", "Sum", , , ]), nrow(d))
})

test_that("missing categories stay distinct from text and count across groups", {
  d <- data.frame(e = c(NA, "NA", "<NA>", NA), o = c(NA, "NA", "x", "x"),
                  g = c("A", NA, "A", NA))
  x <- d |> dplyr::group_by(g) |> freq_xtab(e, o)
  expect_identical(dimnames(x)$e, c("<NA>", "NA", NA, "Sum"))
  expect_identical(dimnames(x)$o, c("NA", "x", NA, "Sum"))
  expect_identical(dimnames(x)$g, c("A", NA))
  expect_equal(x[3, 3, 1], 1)
  expect_equal(x["NA", "NA", 2], 1)
  expect_equal(x[3, "x", 2], 1)
  expect_equal(sum(x["Sum", "Sum", ]), nrow(d))
  expect_identical(freq_xtab(d, e, o, drop = TRUE), freq_xtab(d, e, o))
  numeric_na <- data.frame(e = c(NA, NaN, 1), o = c(TRUE, TRUE, FALSE))
  expect_equal(freq_xtab(numeric_na, e, o)[2, "TRUE"], 2)
  f <- data.frame(e = factor(c("a", NA), exclude = NULL), o = c("x", "x"))
  expect_identical(dimnames(freq_xtab(f, e, o))$e, c("a", NA, "Sum"))
})

test_that("ordering and total labels preserve category identities", {
  d <- data.frame(e = c("Sum", "Sum.1", "Sum.3", NA), o = c(10, 2, -1, NA))
  x <- freq_xtab(d, e, o)
  expect_identical(rownames(x), c("Sum", "Sum.1", "Sum.3", NA, "Sum.2"))
  expect_identical(colnames(x), c("-1", "2", "10", NA, "Sum"))
  expect_equal(x["Sum", "10"], 1)
  expect_equal(x["Sum.2", "Sum"], 4)
  expect_equal(sum(freq_xtab(d, e, o, margins = FALSE)), 4)
  dates <- data.frame(e = as.Date(c("2026-02-01", "2026-01-01", NA)),
                      o = c(FALSE, TRUE, FALSE))
  expect_identical(rownames(freq_xtab(dates, e, o)),
                   c("2026-01-01", "2026-02-01", NA, "Sum"))
  names(d) <- c("n", "odd column")
  expect_identical(names(dimnames(freq_xtab(d, n, `odd column`))), c("n", "odd column"))
  weighted <- transform(xtab_fixture(), n = 100)
  expect_equal(freq_xtab(weighted, exposure, outcome)[3, 3], 10)
})

test_that("empty and singleton dimensions have intentional shapes and zero totals", {
  d <- data.frame(e = character(), o = character())
  expect_identical(freq_xtab(d, e, o), matrix(0, 1, 1,
    dimnames = list(e = "Sum", o = "Sum")))
  expect_identical(dim(freq_xtab(d, e, o, margins = FALSE)), c(0L, 0L))
  factors <- data.frame(e = factor(character(), c("b", "a")),
                        o = factor(character(), c("x", "y")),
                        g = factor(character(), c("A", "B")))
  x <- factors |> dplyr::group_by(g) |> freq_xtab(e, o)
  expect_identical(dim(x), c(3L, 3L, 2L))
  expect_true(all(x == 0))
  z <- factors |> dplyr::group_by(g) |> freq_xtab(e, o, drop = TRUE)
  expect_identical(dim(z), c(1L, 1L, 0L))
  factors$g <- character()
  expect_identical(dim(factors |> dplyr::group_by(g) |> freq_xtab(e, o)), c(3L, 3L, 0L))
  one <- data.frame(e = "a", o = "x", g = "A")
  expect_identical(dim(one |> dplyr::group_by(g) |> freq_xtab(e, o, margins = FALSE)),
                   c(1L, 1L, 1L))
  expect_equal(unname(freq_xtab(one, e, o)), matrix(1, 2, 2))
})

test_that("invalid inputs fail with actionable errors", {
  d <- xtab_fixture()
  expect_error(freq_xtab(1:3, e, o), "data frame")
  expect_error(freq_xtab(d), "both exposure and outcome")
  expect_error(freq_xtab(d, exposure), "both exposure and outcome")
  expect_error(freq_xtab(d, exposure, absent), "not in .data")
  expect_error(freq_xtab(d, exposure, exposure), "distinct")
  expect_error(freq_xtab(d, exposure, outcome, FALSE), "named options")
  expect_error(freq_xtab(d, exposure, outcome, na.rm = TRUE), "named options")
  expect_error(freq_xtab(d, exposure, outcome, margins = NA), "TRUE or FALSE")
  expect_error(freq_xtab(d, exposure, outcome, drop = 1), "TRUE or FALSE")
  expect_error(freq_xtab(d, exposure, outcome, drop = c(TRUE, FALSE)), "TRUE or FALSE")
  expect_error(freq_xtab(d, exposure == "Yes", outcome), "existing columns")
  expect_error(d |> dplyr::group_by(exposure) |> freq_xtab(exposure, outcome), "grouping")
  expect_error(d |> dplyr::group_by(outcome) |> freq_xtab(exposure, outcome), "grouping")
  expect_error(d |> dplyr::rowwise() |> freq_xtab(exposure, outcome), "Rowwise")
  names(d) <- c("e", "e")
  expect_error(freq_xtab(d, e, o), "unique, nonempty")
  d <- data.frame(e = I(list(1, 2)), o = 1:2)
  expect_error(freq_xtab(d, e, o), "categorical vector")
  d <- data.frame(e = I(matrix(1:4, 2)), o = 1:2)
  expect_error(freq_xtab(d, e, o), "categorical vector")
  d <- data.frame(e = c(1, 1 + 1e-15), o = 1:2)
  expect_error(freq_xtab(d, e, o), "identical text labels")
  d <- data.frame(e = factor("x", levels = as.character(1:50000)),
                  o = factor("x", levels = as.character(1:50000)))
  expect_error(freq_xtab(d, e, o), "dense array is too large")
})
