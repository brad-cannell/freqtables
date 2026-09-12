# freqtables Core API Specification

Status: implemented development API, 0.2.0.9000. Decisions finalized with Brad
during the 2026-09-11/12 modernization session. This specification supersedes
the historical sketches in wiki_development_notes.qmd and Development-Notes.md.

## Existing Work Review

- The starting checkout is iss-39-freq-tbl at 3dc7d37. Its freq_tbl() and
  495-line test draft establish grouped counts, factor behavior, and proportion
  output, but retain overall prop by default and lack an export and roxygen.
- iss-40-group-by contains experimental freq_table_v2/v3/v4 variants: alternate
  warning/error transitions, a single-group limit, and a partial compositional
  implementation. They do not satisfy the finalized schema. Useful design
  intent was incorporated without merging obsolete prototypes or generated sites.
- dev-n-way-tables is an exploratory Rmd and comparison-data branch; it is not
  an integrated n-way API. dev-contingency is separate exploratory scope.
- The local meantables implementation is mean_table(), not mean_tables().
  Its grouped-data-plus-one-outcome calling pattern is retained here, without
  importing its two-group limit or generic-only output.
- The untracked CI draft supplies a useful formula starting point but lacks
  ungrouped compact denominators, validation, boundary policy, optional detail,
  and percentage handling. It was completed in R/ci_helpers.R.
- The speculative R/freq_tables.R draft is preserved byte-for-byte at
  dev/freq_tables.R and excluded from package builds; it is not exported.
  The tracked ad hoc freq_tbl_test.R is preserved in dev/ as historical scratch.
- The existing freqtables.Rproj modification is unrelated and is preserved.
- The initial test suite detaches/unloads the package between files, causing
  cascading failures in a clean development session. The tests now use isolated
  testthat blocks and explicit references without attach/detach cleanup.

## Shared Contract

The exported functions are freq_tbl, freq_table, wald_ci, logit_ci, freq_test,
freq_format, and get_group_n. Proposals for multi-outcome tables, contingency
tables, epidemiologic measures, and presentation backends remain outside scope.

Both table constructors take local .data and one .freq_var, with zero or any
number of grouping variables from dplyr::group_by(). A quoted column name and
tidy-evaluation wrapper forwarding are supported. Rowwise data and an outcome
also used as a group are rejected. Options follow an empty ... and must be
named, preventing a second outcome from being mistaken for a logical option.

Canonical option names are drop, percent, overall, generic_col_names, and for
freq_table also percent_ci, ci_type, se, critical_value. Experimental .drop,
.percent, col_headers and generic_col_names alternatives are not aliases.

Default ungrouped schema: outcome, n, prop.
Default grouped schema: groups in supplied order, outcome, n, n_group, prop_group.
freq_table appends lcl/ucl or lcl_group/ucl_group.
Outputs are ungrouped tibbles with freqtables metadata. Category types are
preserved. Reserved statistic names are rejected for analysis columns; unrelated
input columns named n are ignored, never treated as weights.

n is a cell count. n_group is the sum of all outcome counts within a group.
overall=TRUE adds n_total and, for grouped data, prop_total=n/n_total plus
overall intervals in freq_table. This is the joint cell proportion, not an
outcome marginal repeated within every group. Ungrouped overall=TRUE adds
only n_total because prop is already overall.

percent=TRUE renames prop prefixes to percent and scales values by 100.
Intervals and standard errors match that scale; counts and critical values do
not. Counts are the source of interval proportions, validated against display
columns, so percentage conversion is never compounded.

generic_col_names=TRUE creates group_01_col/group_01_cat through group_N
(zero-padded to at least two digits), followed by col/cat. All category values
become character. Stack results only after interval/test computations.

## Missing Data and Factors

NA outcomes and NA group values are categories included in denominators.
Filter observations explicitly before analysis; no na.rm option is added.
drop=FALSE retains unobserved factor levels and empty factor combinations,
following dplyr::count(.drop=FALSE). drop=TRUE uses only observed combinations
and overrides an input group's .drop=FALSE. It does not remove missing values.
Empty groups have zero counts/denominators and NA_real_ proportions.
Empty character inputs yield zero rows; factors can retain zero-count rows.

## Confidence Intervals

Brad explicitly selected continuity with the current Student-t convention:
SE=sqrt(p*(1-p)/(N-1)); t=qt((1+percent_ci/100)/2, df=N-1).
Within-group intervals use n_group for N in BOTH formulas; overall intervals
use n_total. This corrects the previous total-sample df for grouped intervals.
Default confidence level is 95 (percentage units); valid levels lie strictly
between 0 and 100. ci_type is the exact string logit or wald.

wald_ci adds p +/- t*SE without clipping. logit_ci adds t*SE/(p*(1-p)) on the
logit scale and uses plogis for the inverse. At p=0 or p=1, logit bounds are
NA_real_ with no continuity correction; Wald bounds equal p for N>1.
For N<=1, both methods return missing SE, critical values, and bounds.
se and critical_value default to FALSE. Critical columns retain t_crit names.

Helpers accept freqtables results or explicit numeric count/proportion/
denominator columns. Ungrouped compact metadata stores the original total,
avoiding reconstruction from filtered results. Grouped denominators are
explicit columns. Invalid/inconsistent counts, scales, or denominators error.
Helpers cover each included overall/group proportion and replace existing
interval/detail columns on subsequent calls.

## Compatibility and Integration

Legacy freq_table(data, group, outcome) calls fail immediately with a
group_by() replacement message. This is a breaking interface change, not a
deprecation warning with continued execution. A development version bump,
NEWS, migration vignette, and explicit rich-output mapping communicate it.

freq_table classes distinguish zero, one, and multiple groups. freq_test
supports zero or one group, including compact/generic/percentage output.
It checks completeness and uses category identities to build rectangular
contingency tables, including omitted zero cells. The old fixed-two-row Fisher
matrix assumption was corrected. Positive margins and adequate dimensions
are required. Multiple-group tests require a user-defined analysis rather
than an inferred test. ... is explicitly unsupported by test methods.
freq_format works across the new columns and safely handles empty tables
and character categories. get_group_n uses qualified dplyr::n() so it works
without attaching dplyr.

## Fixtures and Verification

The small fixture has A/no=3, A/yes=7, B/no=4, B/yes=2, total=16.
Group totals are 10 and 6; ungrouped counts are 7 and 9. It checks unequal
group degrees of freedom and exact overall versus group denominators.
A deterministic band variable supplies multiple-group examples.
Independent expectations use arithmetic, base table(), stats::qt()/plogis(),
stats::chisq.test(correct=FALSE), and stats::fisher.test(). Tests include
factor/NA/empty data, zero and one proportions, N<=1, generic stacking,
percentage scaling, argument errors, and integration with testing/formatting.

Vignettes cover every export and formal argument, including registered S3
methods, using inst/vignette-coverage.csv. dev/check-vignette-coverage.R checks
the inventory against live signatures and executable section/chunk targets.
See dev/validation.md for executed checks and their results.
