test_that("sample-size labels support no filter, multiple filters, and groups", {
  expect_identical(get_group_n(mtcars), "N = 32")
  expect_identical(get_group_n(mtcars, cyl == 4), "N = 11")
  expect_identical(get_group_n(mtcars, cyl == 4, am == 1), "N = 8")
  expect_identical(get_group_n(mtcars, cyl == 99), "N = 0")
  expect_identical(mtcars |> dplyr::group_by(cyl) |> get_group_n(am == 1),
                   c("N = 8", "N = 3", "N = 2"))
})
