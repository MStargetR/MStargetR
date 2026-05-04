results <- devtools::check(
  ".",
  args = c("--no-manual", "--no-vignettes", "--no-build-vignettes"),
  error_on = "never",
  build_args = c("--no-build-vignettes")
)

cat("\n\n=== R CMD CHECK RESULTS ===\n")
cat("Errors:", length(results$errors), "\n")
cat("Warnings:", length(results$warnings), "\n")
cat("Notes:", length(results$notes), "\n")

if (length(results$errors) > 0) {
  cat("\nERRORS:\n")
  for (e in results$errors) cat(e, "\n---\n")
}
if (length(results$warnings) > 0) {
  cat("\nWARNINGS:\n")
  for (w in results$warnings) cat(w, "\n---\n")
}
if (length(results$notes) > 0) {
  cat("\nNOTES:\n")
  for (n in results$notes) cat(n, "\n---\n")
}
