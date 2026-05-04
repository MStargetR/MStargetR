library(stringr)

sample_names <- c("VLTR_PS_20", "VPQC_21", "PQC_22", "LTR_23",
                   "COV19868_002_24", "COV19831_035_25")

sample_tags <- c("pqc", "vltr", "ltr", "VLTR_PS")

cat("=== BEFORE FIX (greedy str_detect) ===\n")
sample_type_vec <- rep("sample", length(sample_names))
for (tag in rev(sample_tags)) {
  matches <- str_detect(sample_names, regex(tag, ignore_case = TRUE))
  sample_type_vec[matches] <- tag
}
cat(paste(sample_names, "->", sample_type_vec), sep = "\n")

cat("\n=== AFTER FIX (underscore-boundary + longest first + first-match-wins) ===\n")
sample_type_vec2 <- rep("sample", length(sample_names))
sorted_tags <- sample_tags[order(nchar(sample_tags), decreasing = TRUE)]
for (tag in sorted_tags) {
  # Match tag as complete underscore-delimited segment(s)
  # (?:^|_) = preceded by start or underscore
  # (?=_|$) = followed by underscore or end
  pattern <- paste0("(?:^|_)", tag, "(?=_|$)")
  matches <- str_detect(sample_names, regex(pattern, ignore_case = TRUE))
  unclassified <- sample_type_vec2 == "sample"
  sample_type_vec2[matches & unclassified] <- tag
}
cat(paste(sample_names, "->", sample_type_vec2), sep = "\n")
