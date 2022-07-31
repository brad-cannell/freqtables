
<!-- README.md is generated from README.Rmd. Please edit that file -->

# freqtables <img style="padding: 15px;" align="left" src="man/figures/freqtables_hex/freqtables.png" alt="freqtables hex logo" width="250" height="289">

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/freqtables)](https://cran.r-project.org/package=freqtables)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/freqtables)](https://www.r-pkg.org/pkg/freqtables)
<!-- badges: end -->

The goal of `freqtables` is to quickly make tables of descriptive
statistics for categorical variables (i.e., counts, percentages,
confidence intervals). This package is designed to work in a `tidyverse`
pipeline, and consideration has been given to get results from R to
Microsoft Word ® with minimal pain.

## Installation

You can install the released version of `freqtables` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("freqtables")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("brad-cannell/freqtables")
```

## Example

Because `freqtables` is intended to be used in a `dplyr` pipeline,
loading `dplyr` into your current R session is recommended.

``` r
library(dplyr)
library(freqtables)
```

The examples below will use R’s built-in `mtcars` data set.

``` r
data("mtcars")
```

### freq_table()

The `freq_table()` function produces one-way and two-way frequency
tables for categorical variables. In addition to frequencies, the
`freq_table()` function displays percentages, and the standard errors
and confidence intervals of the percentages. For two-way tables only,
`freq_table()` also displays row (subgroup) percentages, standard
errors, and confidence intervals.

For one-way tables, the default 95 percent confidence intervals
displayed are logit transformed confidence intervals equivalent to those
used by Stata. Additionally, `freq_table()` will return Wald (“linear”)
confidence intervals if the argument to ci_type = “wald”.

For two-way tables, `freq_table()` returns logit transformed confidence
intervals equivalent to those used by Stata.

Here is an example of using `freq_table()` to create a one-way frequency
table with all function arguments left at their default values:

``` r
mtcars %>% 
  freq_table(am)
#>   var cat  n n_total percent       se   t_crit      lcl      ucl
#> 1  am   0 19      32  59.375 8.820997 2.039513 40.94225 75.49765
#> 2  am   1 13      32  40.625 8.820997 2.039513 24.50235 59.05775
```

Here is an example of using `freq_table()` to create a two-way frequency
table with all function arguments left at their default values:

``` r
mtcars %>% 
  group_by(am) %>% 
  freq_table(cyl)
#> # A tibble: 3 × 9
#>   var   cat       n n_total percent    se t_crit   lcl   ucl
#>   <chr> <chr> <int>   <int>   <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1 cyl   4        11      32    34.4  8.53   2.04  19.5  53.1
#> 2 cyl   6         7      32    21.9  7.42   2.04  10.3  40.4
#> 3 cyl   8        14      32    43.8  8.91   2.04  27.1  61.9
```

You can learn more about the `freq_table()` function and ways to adjust
default behaviors in vignette(“descriptive_analysis”).

### freq_test()

The `freq_test()` function is an S3 generic. It currently has methods
for conducting hypothesis tests on one-way and two-way frequency tables.
Further, it is made to work in a dplyr pipeline with the `freq_table()`
function.

For the `freq_table_two_way` class, the methods used are Pearson’s
chi-square test of independence Fisher’s exact test. When cell counts
are \<= 5, Fisher’s Exact Test is considered more reliable.

Here is an example of using `freq_test()` to test the equality of
proportions on a one-way frequency table with all function arguments
left at their default values:

``` r
mtcars %>%
  freq_table(am) %>%
  freq_test() %>%
  select(var:percent, p_chi2_pearson)
#>   var cat  n n_total percent p_chi2_pearson
#> 1  am   0 19      32  59.375      0.2888444
#> 2  am   1 13      32  40.625      0.2888444
```

Here is an example of using `freq_test()` to conduct a chi-square test
of independence on a two-way frequency table with all function arguments
left at their default values:

``` r
mtcars %>%
  freq_table(am, vs) %>%
  freq_test() %>%
  select(row_var:n, percent_row, p_chi2_pearson)
#> # A tibble: 4 × 7
#>   row_var row_cat col_var col_cat     n percent_row p_chi2_pearson
#>   <chr>   <chr>   <chr>   <chr>   <int>       <dbl>          <dbl>
#> 1 am      0       vs      0          12        63.2          0.341
#> 2 am      0       vs      1           7        36.8          0.341
#> 3 am      1       vs      0           6        46.2          0.341
#> 4 am      1       vs      1           7        53.8          0.341
```

You can learn more about the `freq_table()` function and ways to adjust
default behaviors in vignette(“using_freq_test”).

### freq_format()

The freq_format function is intended to make it quick and easy to format
the output of the freq_table function for tables that may be used for
publication. For example, a proportion and 95% confidence interval could
be formatted as “24.00 (21.00 - 27.00).”

``` r
mtcars %>%
  freq_table(am) %>%
  freq_format(
    recipe = "percent (lcl - ucl)",
    name = "percent_95",
    digits = 2
  ) %>%
  select(var, cat, percent_95)
#>   var cat            percent_95
#> 1  am   0 59.38 (40.94 - 75.50)
#> 2  am   1 40.62 (24.50 - 59.06)
```

You can learn more about the `freq_format()` function by reading the
function documentation.
