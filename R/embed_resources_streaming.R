#' Stream-embed external resources into an rmarkdown HTML report
#'
#' R-side replacement for pandoc's `--embed-resources` pass. Reads the input
#' HTML line by line, inlines each `<link>`/`<script>`/`<img>` reference that
#' points into the report's `_files/` sidecar, and writes incrementally to
#' disk. Peak working memory is bounded by the largest individual resource
#' (typically the plotly main bundle, ~5 MB) instead of the assembled HTML,
#' which on a 50-plate cohort can exceed the Windows commit limit when handed
#' to pandoc and trigger `osCommitMemory: VirtualAlloc MEM_COMMIT failed`.
#'
#' Produces a self-contained HTML that is functionally equivalent to
#' `rmarkdown::render(..., output_options = list(self_contained = TRUE))`.
#'
#' @param html_path Path to the unembedded HTML file (i.e. rendered with
#'   `self_contained = FALSE`). The file is rewritten in place.
#' @param files_dir Optional explicit path to the `_files/` sidecar. Defaults
#'   to `<basename without extension>_files` next to the HTML file.
#' @param delete_files_dir Logical; remove the sidecar directory once
#'   embedding succeeds. Default `TRUE`.
#'
#' @return Invisibly returns the path to the now self-contained HTML.
#' @keywords internal
#' @noRd
embed_resources_streaming <- function(html_path,
                                      files_dir = NULL,
                                      delete_files_dir = TRUE) {
  if (!file.exists(html_path)) {
    stop("embed_resources_streaming: HTML file not found: ", html_path,
         call. = FALSE)
  }

  html_path <- normalizePath(html_path, mustWork = TRUE, winslash = "/")
  html_dir  <- dirname(html_path)

  if (is.null(files_dir)) {
    base <- tools::file_path_sans_ext(basename(html_path))
    files_dir <- file.path(html_dir, paste0(base, "_files"))
  }

  tmp_path <- tempfile(tmpdir = html_dir, fileext = ".html")
  con_in   <- file(html_path, open = "r", encoding = "UTF-8")
  con_out  <- file(tmp_path,  open = "w", encoding = "UTF-8")

  on.exit({
    try(close(con_in),  silent = TRUE)
    try(close(con_out), silent = TRUE)
    if (file.exists(tmp_path)) unlink(tmp_path)
  }, add = TRUE)

  repeat {
    line <- readLines(con_in, n = 1L, warn = FALSE, encoding = "UTF-8")
    if (length(line) == 0L) break
    Encoding(line) <- "UTF-8"
    line <- embed_refs_in_line(line, html_dir)
    Encoding(line) <- "UTF-8"
    writeLines(line, con_out, useBytes = TRUE)
  }

  close(con_in)
  close(con_out)
  if (!file.copy(tmp_path, html_path, overwrite = TRUE)) {
    stop("embed_resources_streaming: failed to overwrite ", html_path,
         call. = FALSE)
  }
  unlink(tmp_path)

  if (isTRUE(delete_files_dir) && dir.exists(files_dir)) {
    unlink(files_dir, recursive = TRUE, force = TRUE)
  }

  invisible(html_path)
}

# Replace per-line <link>/<script>/<img> tags whose href/src points into the
# local `_files/` sidecar. External URLs and broken refs are passed through
# unchanged.
#' @noRd
embed_refs_in_line <- function(line, html_dir) {
  # Cheap early exit: most lines have no embeddable tag.
  if (!grepl("<(link|script|img)\\b", line, perl = TRUE)) return(line)

  line <- replace_tag_with_local_ref(
    line,
    pattern = '<link\\b[^>]*\\bhref="([^"]+)"[^>]*/?>',
    replacer = function(match, ref) inline_link_tag(match, ref, html_dir)
  )

  line <- replace_tag_with_local_ref(
    line,
    pattern = '<script\\b[^>]*\\bsrc="([^"]+)"[^>]*></script>',
    replacer = function(match, ref) inline_script_tag(match, ref, html_dir)
  )

  line <- replace_tag_with_local_ref(
    line,
    pattern = '<img\\b[^>]*\\bsrc="([^"]+)"[^>]*/?>',
    replacer = function(match, ref) inline_img_tag(match, ref, html_dir)
  )

  line
}

# Apply `replacer(match_text, captured_ref)` to every regex match in `line`.
# Order-preserving and safe for multiple matches per line.
#' @noRd
replace_tag_with_local_ref <- function(line, pattern, replacer) {
  m <- gregexpr(pattern, line, perl = TRUE)
  matches <- regmatches(line, m)[[1]]
  if (length(matches) == 0L) return(line)

  replacements <- vapply(matches, function(match) {
    sub_m <- regexec(pattern, match, perl = TRUE)
    caps <- regmatches(match, sub_m)[[1]]
    ref <- if (length(caps) >= 2L) caps[2] else NA_character_
    if (is.na(ref)) return(match)
    replacer(match, ref)
  }, character(1), USE.NAMES = FALSE)

  regmatches(line, m) <- list(replacements)
  line
}

# A URL we must not try to inline from the local filesystem.
#' @noRd
is_remote_ref <- function(ref) {
  if (!nzchar(ref)) return(TRUE)
  grepl("^(?:[a-z][a-z0-9+.\\-]*:)", ref, perl = TRUE, ignore.case = TRUE) ||
    startsWith(ref, "//") ||
    startsWith(ref, "#")
}

# Resolve a relative ref against `html_dir`, returning NULL if it doesn't
# exist on disk.
#' @noRd
resolve_local_ref <- function(ref, html_dir) {
  if (is_remote_ref(ref)) return(NULL)
  clean <- sub("[#?].*$", "", ref)
  path <- file.path(html_dir, clean)
  if (!file.exists(path)) return(NULL)
  path
}

#' @noRd
inline_link_tag <- function(match, ref, html_dir) {
  path <- resolve_local_ref(ref, html_dir)
  if (is.null(path)) return(match)
  if (tolower(tools::file_ext(path)) != "css") return(match)
  css <- read_text_utf8(path)
  css <- inline_css_urls(css, dirname(path))
  paste0("<style>", css, "</style>")
}

#' @noRd
inline_script_tag <- function(match, ref, html_dir) {
  path <- resolve_local_ref(ref, html_dir)
  if (is.null(path)) return(match)
  js <- read_text_utf8(path)
  # Strip the src attribute but keep other attrs (defer, async, type, ...).
  open_tag <- sub('\\s*\\bsrc="[^"]+"', "", match, perl = TRUE)
  open_tag <- sub('></script>$', '>', open_tag, perl = TRUE)
  paste0(open_tag, js, "</script>")
}

#' @noRd
inline_img_tag <- function(match, ref, html_dir) {
  path <- resolve_local_ref(ref, html_dir)
  if (is.null(path)) return(match)
  data_uri <- file_to_data_uri(path)
  sub('\\bsrc="[^"]+"', paste0('src="', data_uri, '"'),
      match, perl = TRUE)
}

# Rewrite url(...) references inside an inlined CSS body to data: URIs.
#' @noRd
inline_css_urls <- function(css, css_dir) {
  pattern <- "url\\(\\s*(?:\"([^\"]+)\"|'([^']+)'|([^'\"\\)\\s]+))\\s*\\)"
  m <- gregexpr(pattern, css, perl = TRUE)
  matches <- regmatches(css, m)[[1]]
  if (length(matches) == 0L) return(css)

  replacements <- vapply(matches, function(match) {
    sub_m <- regexec(pattern, match, perl = TRUE)
    caps <- regmatches(match, sub_m)[[1]][-1]
    url <- caps[nzchar(caps)][1]
    if (is.na(url) || is_remote_ref(url)) return(match)
    clean <- sub("[#?].*$", "", url)
    path <- file.path(css_dir, clean)
    if (!file.exists(path)) return(match)
    paste0("url(", file_to_data_uri(path), ")")
  }, character(1), USE.NAMES = FALSE)

  regmatches(css, m) <- list(replacements)
  css
}

# Read a small-to-medium text file as a single UTF-8 string. Used for CSS/JS
# resources whose typical size is well under the streaming threshold; the
# whole-file read here is bounded by individual resource size (the largest
# being plotly's main bundle at ~5 MB), not by the assembled HTML size.
#' @noRd
read_text_utf8 <- function(path) {
  size <- file.info(path)$size
  if (is.na(size) || size == 0) return("")
  raw <- readBin(path, what = "raw", n = size)
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  txt
}

# Encode a binary resource as a data: URI. Delegates to xfun (a hard dep of
# knitr/rmarkdown, so guaranteed available on the render path).
#' @noRd
file_to_data_uri <- function(path) {
  if (!requireNamespace("xfun", quietly = TRUE)) {
    stop("Package 'xfun' is required for resource embedding.", call. = FALSE)
  }
  xfun::base64_uri(path)
}
