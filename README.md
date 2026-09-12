
<!-- README.md is generated from README.Rmd. Please edit that file -->

# freqtables

Quick, composable frequency tables for categorical variables in dplyr
pipelines. Use `freq_tbl()` for counts and proportions, `wald_ci()` or
`logit_ci()` to add intervals, and `freq_table()` to do both.

This checkout contains the **0.2.0.9000 development API**. It changes
grouping syntax and output defaults from 0.1.1. The code shown here
applies to this development version; the currently released version may
have the previous API.

## Installation

Install the released version with:

``` r
install.packages("freqtables")
```

To use this checkout’s development API, build and install the local
package after its dependencies are available, or install the development
branch you intend to use. These changes are prepared locally and have
not been published.

## Counts and Intervals

``` r
freq_tbl(mtcars, am)
#> # A tibble: 2 × 3
#>      am     n  prop
#>   <dbl> <int> <dbl>
#> 1     0    19 0.594
#> 2     1    13 0.406
mtcars |> group_by(cyl) |> freq_tbl(am)
#> # A tibble: 6 × 5
#>     cyl    am     n n_group prop_group
#>   <dbl> <dbl> <int>   <int>      <dbl>
#> 1     4     0     3      11      0.273
#> 2     4     1     8      11      0.727
#> 3     6     0     4       7      0.571
#> 4     6     1     3       7      0.429
#> 5     8     0    12      14      0.857
#> 6     8     1     2      14      0.143
mtcars |> group_by(cyl) |> freq_table(am)
#> # A tibble: 6 × 7
#>     cyl    am     n n_group prop_group lcl_group ucl_group
#>   <dbl> <dbl> <int>   <int>      <dbl>     <dbl>     <dbl>
#> 1     4     0     3      11      0.273    0.0716     0.646
#> 2     4     1     8      11      0.727    0.354      0.928
#> 3     6     0     4       7      0.571    0.150      0.909
#> 4     6     1     3       7      0.429    0.0906     0.850
#> 5     8     0    12      14      0.857    0.520      0.971
#> 6     8     1     2      14      0.143    0.0292     0.480
```

One outcome is supplied to the function; `group_by()` provides any
grouping variables. Default results use proportions. Grouped tables
include cell counts (`n`), group totals (`n_group`), and within-group
proportions (`prop_group`). Overall cell proportions are optional.

``` r
mtcars |> group_by(cyl, vs) |>
  freq_table(am, percent = TRUE, overall = TRUE)
#> # A tibble: 7 × 12
#>     cyl    vs    am     n n_group percent_group n_total percent_total lcl_group
#>   <dbl> <dbl> <dbl> <int>   <int>         <dbl>   <int>         <dbl>     <dbl>
#> 1     4     0     1     1       1         100        32          3.12     NAn#> 2     4     1     0     3      10          30        32          9.38      7.64
#> 3     4     1     1     7      10          70        32         21.9      31.0n#> 4     6     0     1     3       3         100        32          9.38     NAn#> 5     6     1     0     4       4         100        32         12.5      NAn#> 6     8     0     0    12      14          85.7      32         37.5      52.0n#> 7     8     0     1     2      14          14.3      32          6.25      2.92
#> # ℹ 3 more variables: ucl_group <dbl>, lcl_total <dbl>, ucl_total <dbl>
freq_tbl(mtcars, am) |> wald_ci(percent_ci = 99)
#> # A tibble: 2 × 5
#>      am     n  prop   lcl   ucl
#>   <dbl> <int> <dbl> <dbl> <dbl>
#> 1     0    19 0.594 0.352 0.836
#> 2     1    13 0.406 0.164 0.648
```

The t-based interval convention is preserved. Each group’s standard
error and critical value now both use its own sample size. See the
interval vignette for boundary behavior and formulas.

## Tests and Formatting

``` r
mtcars |> group_by(am) |>
  freq_table(cyl, percent = TRUE) |>
  freq_test() |>
  freq_format("n (percent_group%)", name = "estimate", digits = 1) |>
  select(am, cyl, estimate, p_chi2_pearson)
#> One or more expected cell counts are <= 5. Fisher's exact p-value is also returned.
#> # A tibble: 6 × 4
#>      am   cyl estimate   p_chi2_pearson
#>   <dbl> <dbl> <chr>               <dbl>
#> 1     0     4 3 (15.8%)          0.0126
#> 2     0     6 4 (21.1%)          0.0126
#> 3     0     8 12 (63.2%)         0.0126
#> 4     1     4 8 (61.5%)          0.0126
#> 5     1     6 3 (23.1%)          0.0126
#> 6     1     8 2 (15.4%)          0.0126

get_group_n(mtcars, cyl == 4)
#> [1] "N = 11"
```

Filter missing observations explicitly before counting when they should
not contribute to denominators. Generic headers are available with
`generic_col_names = TRUE`; use them to stack completed analyses.

## Learn Every Function and Argument

Build/install the package with vignettes, then open:

- `vignette("descriptive_analysis", package = "freqtables")`: inputs,
  groups, missing values, factors, and every table-constructor option.
- `vignette("confidence-intervals", package = "freqtables")`: both
  helpers, formulas, scales, confidence levels, and boundary cases.
- `vignette("using_freq_test", package = "freqtables")`: one-way/two-way
  methods, assumptions, unsupported arguments, and complete pipelines.
- `vignette("formatting", package = "freqtables")`: recipes, decimal
  places, column names, and all sample-size filter options.
- `vignette("migration", package = "freqtables")`: old/new syntax and
  columns.

Source files are in [vignettes/](vignettes/). The exact
function/argument mapping is
[inst/vignette-coverage.csv](inst/vignette-coverage.csv); development
checks compare it with live signatures. API decisions are recorded in
[dev/api-specification.md](dev/api-specification.md).

## Migration from 0.1.1

Replace `freq_table(mtcars, cyl, am)` with
`mtcars |> group_by(cyl) |> freq_table(am)`. Old multiple-variable calls
now error with this guidance. Options must be named. Use
`percent = TRUE` for percentage-scale output, and request `overall`,
`se`, and `critical_value` when needed. A rich generic table is
available without changing the compact defaults:

``` r
mtcars |> group_by(cyl) |>
  freq_table(am, percent = TRUE, overall = TRUE, se = TRUE,
             critical_value = TRUE, generic_col_names = TRUE)
#> # A tibble: 6 × 17
#>   group_01_col group_01_cat col   cat       n n_group percent_group n_total
#>   <chr>        <chr>        <chr> <chr> <int>   <int>         <dbl>   <int>
#> 1 cyl          4            am    0         3      11          27.3      32
#> 2 cyl          4            am    1         8      11          72.7      32
#> 3 cyl          6            am    0         4       7          57.1      32
#> 4 cyl          6            am    1         3       7          42.9      32
#> 5 cyl          8            am    0        12      14          85.7      32
#> 6 cyl          8            am    1         2      14          14.3      32
#> # ℹ 9 more variables: percent_total <dbl>, se_group <dbl>, t_crit_group <dbl>,
#> #   lcl_group <dbl>, ucl_group <dbl>, se_total <dbl>, t_crit_total <dbl>,
#> #   lcl_total <dbl>, ucl_total <dbl>
```
