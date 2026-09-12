# Validation of the freqtables Core Redesign

Verified 2026-09-11 (America/Chicago) on the isolated copy of
iss-39-freq-tbl, starting from 3dc7d37. The verified source changes were
then applied to the original local checkout without changing its branch
or the unrelated freqtables.Rproj modification.

## Results

- R CMD build --no-manual: successful, with all five vignettes built.
- R CMD check --no-manual on the source tarball: Status OK;
  **0 errors, 0 warnings, 0 notes**.
- Installed-package tests: **211 passed, 0 failed, 0 warnings, 0 skipped**.
- Development tests via testthat::test_local(): all passed.
- Roxygen regeneration, Rd cross-references, example execution, namespace/
  dependency checks, code/documentation signatures, and S3 consistency: passed.
- All five installed vignettes rebuilt during R CMD check.
- Coverage audit: 7 exports, 3 registered S3 methods, 40 formal argument
  mappings, each targeting an existing vignette section and executable chunk.
- Rendered HTML titles and code output inspected for all five vignettes;
  numerical results checked through independent fixtures, direct R reference
  calculations, and executable vignette assertions.
- README.Rmd rendered successfully with the verified development package.
- git diff --check: passed.

The full final check log is check-2026-09-11.log alongside this report.

## Environment and Commands

R 4.5.2, macOS Sequoia 15.7.3 (arm64), dplyr 1.2.0, rlang 1.1.7,
testthat 3.2.3, roxygen2 7.3.3, knitr 1.51, rmarkdown 2.30,
Pandoc 3.8.3 supplied by the existing Quarto installation.

From the package root:

```r
roxygen2::roxygenise()
testthat::test_local()
pkgload::load_all()
source("dev/check-vignette-coverage.R")
rmarkdown::find_pandoc(dir = "/Applications/quarto/bin/tools/aarch64")
rmarkdown::render("README.Rmd")
```

From a temporary parent directory:

```sh
RSTUDIO_PANDOC=/Applications/quarto/bin/tools/aarch64 R CMD build --no-manual freqtables
RSTUDIO_PANDOC=/Applications/quarto/bin/tools/aarch64 R CMD check --no-manual freqtables_0.2.0.9000.tar.gz
```

The final check had network access for dependency repository indexes.
Earlier restricted checks could not access those indexes; that limitation
was resolved for the final run. Build-time tar uid/gid normalization and
Pandoc's deprecated-highlight-option messages are tool/environment messages;
the final R CMD check reported no warnings or notes.

## Corrections Discovered During Verification

The initial test suite unloaded freqtables between files and could not run
coherently in a clean development session. Test isolation was corrected.
get_group_n() depended on an unqualified n() from an attached dplyr;
it now uses dplyr::n(). A one-column empty-data vignette example needed
drop=FALSE. An unparseable NEWS heading was corrected before the final check.

Ungrouped confidence limits retain the previous numerical results.
Grouped limits use each group's denominator and t degrees of freedom;
independent unequal-group fixtures verify this difference explicitly.
Fisher tests are checked against stats::fisher.test() for both 2-by-3
and 3-by-2 tables and shuffled table rows.

## Scope and Handoff

The changes are local and uncommitted on iss-39-freq-tbl. No release,
CRAN submission, remote push, or global package installation was performed.
The PDF reference manual was not built (--no-manual); all Rd validation,
examples, tests, and HTML vignette checks were run.

R/freq_tables.R was preserved byte-for-byte at dev/freq_tables.R and the
old ad hoc R/freq_tbl_test.R at dev/freq_tbl_test.R (subsequently moved to
dev/archive/freq_tbl_test.R during repository cleanup). Neither is packaged.
The existing CI draft was completed, with its original t-based convention
retained. The original .Rproj change remains untouched.
