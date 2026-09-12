# freqtables 0.2.0.9000

* Breaking API: freq_table() accepts one outcome with groups supplied through
  dplyr::group_by(); legacy two-variable calls now error with migration guidance.
  Proportions and compact columns replace verbose percentage output by default.
* Export freq_tbl(), wald_ci(), and logit_ci(), with consistent grouped and
  ungrouped schemas, named options, explicit missing-data handling, and optional
  percentages, overall results, generic headers, standard errors, and t values.
* Preserve the existing t-based interval convention; within-group standard
  errors and critical values now both use n_group - 1. Logit bounds at zero/one
  and intervals with denominators <= 1 are explicitly missing.
* Keep freq_test() and freq_format() compatible with compact/generic results.
  Construct Fisher matrices by category identities for rectangular tables,
  validate complete test inputs, and reject unsupported extra arguments.
* Fix get_group_n() to work without attaching dplyr; preserve formatting
  prefixes and safely format empty tables and character categories.
* Add five comprehensive vignettes, a function/argument coverage inventory,
  migration guidance, and independent numerical/integration tests.
* Require R >= 4.1.0 and dplyr >= 1.0.0 for the documented pipeline syntax.
  This is an unpublished development version, not a CRAN release.

* Add built-in example study data to freqtables

# freqtables 0.1.1

* Add an informative error for user if they forget to pass a data frame to freq_table() (#29)

# freqtables 0.1.0

* First release on CRAN
