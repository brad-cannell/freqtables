# Validation of freq_xtab

Verified 2026-09-11 (America/Chicago), using a temporary copy of
iss-39-freq-tbl at 71d3a94. The validated files are applied to the original
checkout individually; the pre-existing freqtables.Rproj edit is preserved.

## Results

- Installed-package tests: **288 passed, 0 failed, 0 warnings, 0 skipped**,
  including 77 new contingency expectations and all 211 existing expectations.
- R CMD build --no-manual: successful; all six HTML vignettes built.
- R CMD check --no-manual: **Status OK; 0 errors, 0 warnings, 0 notes**.
  Examples, dependency declarations, R code analysis, Rd validation, S3
  consistency, installed tests, and rebuilding all six vignettes passed.
- Roxygen regeneration and the development test suite passed.
- Coverage audit: 8 exports, 3 S3 methods, 46 formal argument mappings;
  every mapping targets an existing vignette section and executable chunk.
- README.Rmd rendered successfully. An existing output-hook replacement
  incorrectly printed literal n instead of newlines; the source hook was
  corrected, restoring separate rows in the regenerated README output.
- The built contingency vignette contains the exact source matrix and totals.
  Executable assertions verify its counts, dimensions, empty slices, retained
  levels, and group totals. Its rendered text and example output were inspected.
- Independent tests compare asymmetric 2-by-3 tables and arrays with up to
  three grouping variables against base table()/addmargins(). They cover
  reordered input, missing values and literal labels, empty factors, globally
  absent versus group-specific absent categories, margin-label collisions,
  singleton dimensions, invalid calls, and oversized array rejection.

The check log is contingency-check-2026-09-11.log alongside this report.
The check used installed dependencies; the sandbox could not retrieve online
CRAN/Bioconductor repository indexes. The dependency check still reported OK.
Build-time tar uid/gid normalization messages did not affect the check status.
The PDF reference manual was not built (--no-manual).

## Commands and Environment

R 4.5.2 on macOS Sequoia 15.7.3 (arm64); Pandoc 3.8.3 from Quarto.

```r
roxygen2::roxygenise()
testthat::test_local()
pkgload::load_all()
source("dev/check-vignette-coverage.R")
rmarkdown::find_pandoc(dir = "/Applications/quarto/bin/tools/aarch64")
rmarkdown::render("README.Rmd")
```

From the temporary package parent:

```sh
RSTUDIO_PANDOC=/Applications/quarto/bin/tools/aarch64 R CMD build --no-manual freqtables
RSTUDIO_PANDOC=/Applications/quarto/bin/tools/aarch64 R CMD check --no-manual freqtables_0.2.0.9000.tar.gz
```

## Handoff

The source, tests, generated help/export, vignette, README, NEWS, coverage
inventory, specification, and validation record are local changes. The branch
and its prior commits are preserved; no release, push, or global package
installation was performed. The approved task covers count matrices and arrays;
epidemiologic measures and new display or statistical integrations remain
separate work.
