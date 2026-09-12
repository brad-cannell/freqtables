# Run from the package root after pkgload::load_all() or library(freqtables).
inventory <- read.csv("inst/vignette-coverage.csv", check.names = FALSE,
                       stringsAsFactors = FALSE)
namespace <- asNamespace("freqtables")
exports <- getNamespaceExports(namespace)
exports <- exports[vapply(exports, function(x) is.function(get(x, namespace)), logical(1))]
methods <- getNamespaceInfo(namespace, "S3methods")
method_functions <- paste(methods[, 1], methods[, 2], sep = ".")
functions <- sort(unique(c(exports, method_functions)))
stopifnot(setequal(functions, inventory[["function"]]))
for (fn in functions) {
  signature <- names(formals(get(fn, namespace)))
  documented <- inventory$argument[inventory[["function"]] == fn]
  if (!setequal(signature, documented) || anyDuplicated(documented)) {
    stop("Vignette argument coverage differs from signature: ", fn)
  }
}
for (i in seq_len(nrow(inventory))) {
  source <- readLines(paste0("vignettes/", inventory$vignette[i], ".Rmd"),
                       warn = FALSE)
  anchor <- paste0("{#", inventory$section[i], "}")
  chunk <- paste0("\x60\x60\x60{r ", inventory$chunk[i])
  if (!any(grepl(anchor, source, fixed = TRUE))) stop("Missing section: ", anchor)
  # Match the complete chunk label, not a prefix of another label.
  if (!any(startsWith(source, paste0(chunk, "}")) |
           startsWith(source, paste0(chunk, ",")))) stop("Missing chunk: ", chunk)
}
cat(length(exports), "exports,", length(method_functions),
    "S3 methods,", nrow(inventory), "argument mappings verified.\n")
