# Contingency Table API Specification

Status: approved by Brad and implemented in the 0.2.0.9000 development API.

## Existing Work Review

The current base is iss-39-freq-tbl at 71d3a94, containing the committed core
redesign. Its only pre-existing working-tree modification is freqtables.Rproj.
The dev-contingency branch at b664ab3 adds two exploratory R Markdown notebooks:
dev_contingency.Rmd and vignettes/dev_contingency_ggplot.Rmd. They contain a
manually entered matrix, a table_margins() helper, and a ggplot sketch, but no
freq_xtab() implementation, export, grouped-array behavior, or tests. They
remain on their original branch. The new implementation uses the current
package's input conventions and flag validator, without merging old prototypes.

## Public Contract

```r
freq_xtab(.data, exposure, outcome, ..., margins = TRUE, drop = FALSE)
```

- Local data frames and grouped data frames are supported. Two distinct bare
  names or literal strings identify exposure and outcome; {{ }} forwarding
  works in wrappers. Derived expressions, rowwise input, duplicated or empty
  input column names, missing columns, nonempty dots, and invalid flags error.
  Neither analysis column may also be a grouping variable.
- Ungrouped output is a numeric matrix. Grouped output is a numeric array with
  axes exposure, outcome, then every group in group_by() order. Axis names
  preserve original column names, including non-syntactic names. This function
  does not create statistic columns, so names such as n are valid axis names.
- Input rows are observations, not aggregated counts or weights. Every row
  contributes one cell count, including missing values. NA and NaN share the
  missing category. Filter explicitly to exclude observations.
- Factor order follows levels. Other categories sort naturally, with character
  values in C-locale order. Missing categories come last, before any margin.
  No automatic positive/reference-category ordering is imposed.
- Labels are character conversions of category values. Actual NA labels remain
  distinct from literal NA or <NA> strings. Distinct values with identical
  converted labels error, rather than silently merging categories. One-dimensional
  atomic categorical vectors are supported; list, matrix, raw, and complex
  columns are rejected.
- drop=FALSE retains unused factor levels on all axes. drop=TRUE removes levels
  unused in the entire input, overriding the input grouping's .drop setting.
  Both options represent every combination of retained categories and fill
  absent combinations with zero. This rectangular-array rule differs from
  freq_tbl(drop=TRUE), which can omit rows for unobserved cell combinations.
- margins=TRUE appends totals to the first two axes only. Each group slice has
  its own row/column totals and grand total; no margins cross grouping axes.
  The label is Sum, or the first available Sum.1, Sum.2, etc. independently on
  each axis. Existing category labels are never renamed. margins=FALSE returns
  only the cell counts.
- Zero and singleton dimensions are retained. Empty factors retain their
  levels with drop=FALSE, and empty nonfactor axes have no categories. Empty
  ungrouped character input returns 1-by-1 zero with margins, 0-by-0 without.
  An empty grouping axis produces no matrix slices. R subsetting can simplify
  dimensions; pass drop=FALSE to [ when preserving the full shape is needed.
- Allocation is dense. Shapes exceeding the integer indexing limit error
  before allocation. Smaller arrays can still exhaust available memory.

## Implementation and Verification

Counts use category identities and column-major array indices. Margins count
the same observations at the two total positions and their intersection.
This handles zero and singleton dimensions without implicit simplification.
No new package dependency, S3 class, or integration with existing test/interval/
formatting methods is introduced.

The source fixture has cells 3, 2, 3, 2, row totals 5/5, column totals 6/4,
and grand total 10. An asymmetric 2-by-3 fixture with counts 1 through 6
detects category/axis errors. Independent base table()/addmargins() references
check ungrouped and multiple-group arrays. Focused tests also cover argument
handling, ordering, NA/text distinctions, unused levels, label collisions,
empty data, singleton groups, and excessive shapes. The executable vignette
contingency-tables.Rmd covers every formal argument via the coverage inventory.
See contingency-validation.md for checks actually executed.

Epidemiologic measures, table conversions, and new display formats remain
separate tasks. freq_test(), confidence-interval helpers, and freq_format()
retain their existing frequency-table inputs.
