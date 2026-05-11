# Tests for embed_resources_streaming() ----
#
# The function is an R-side replacement for pandoc's --embed-resources pass.
# It must produce a self-contained HTML with bounded peak memory: each
# referenced resource is read, encoded, and written individually so the
# assembled HTML never sits in RAM as one giant string.

# Helper: build a tiny report layout (html + sidecar files) in tempdir() ----
make_fake_report <- function(html_lines, files_layout = list()) {
  tmp_dir <- tempfile("emb_")
  dir.create(tmp_dir)
  html_path <- file.path(tmp_dir, "report.html")
  files_dir <- file.path(tmp_dir, "report_files")
  dir.create(files_dir)

  for (rel_path in names(files_layout)) {
    full <- file.path(files_dir, rel_path)
    dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
    content <- files_layout[[rel_path]]
    if (is.raw(content)) {
      writeBin(content, full)
    } else {
      writeLines(content, full, useBytes = TRUE)
    }
  }

  writeLines(html_lines, html_path, useBytes = TRUE)
  list(html_path = html_path, files_dir = files_dir, tmp_dir = tmp_dir)
}

# ---------------------------------------------------------------------------

test_that("embed_resources_streaming inlines local <script> and <link>", {
  skip_if_not_installed("xfun")

  layout <- make_fake_report(
    html_lines = c(
      "<!doctype html><html><head>",
      '<link rel="stylesheet" href="report_files/style.css" />',
      '<script src="report_files/app.js"></script>',
      "</head><body><p>hello</p></body></html>"
    ),
    files_layout = list(
      "style.css" = ".x { color: red; }",
      "app.js"    = "console.log('local-js');"
    )
  )
  on.exit(unlink(layout$tmp_dir, recursive = TRUE), add = TRUE)

  embed_resources_streaming(layout$html_path)

  out <- paste(readLines(layout$html_path), collapse = "\n")

  expect_match(out, "<style>\\.x \\{ color: red; \\}\\s*</style>")
  expect_match(out, "<script>console\\.log\\('local-js'\\);\\s*</script>")
  expect_false(grepl("report_files/", out, fixed = TRUE))
  expect_false(dir.exists(layout$files_dir))
})

test_that("embed_resources_streaming leaves remote URLs untouched", {
  skip_if_not_installed("xfun")

  layout <- make_fake_report(
    html_lines = c(
      "<!doctype html><html><head>",
      '<script src="https://cdn.example.com/remote.js"></script>',
      '<link rel="stylesheet" href="//cdn.example.com/x.css" />',
      "</head></html>"
    )
  )
  on.exit(unlink(layout$tmp_dir, recursive = TRUE), add = TRUE)

  embed_resources_streaming(layout$html_path)

  out <- paste(readLines(layout$html_path), collapse = "\n")
  expect_match(out, "https://cdn.example.com/remote.js", fixed = TRUE)
  expect_match(out, "//cdn.example.com/x.css", fixed = TRUE)
})

test_that("embed_resources_streaming handles missing files without erroring", {
  skip_if_not_installed("xfun")

  layout <- make_fake_report(
    html_lines = c(
      "<!doctype html><html><head>",
      '<link href="report_files/missing.css" rel="stylesheet" />',
      '<script src="report_files/missing.js"></script>',
      "</head></html>"
    )
  )
  on.exit(unlink(layout$tmp_dir, recursive = TRUE), add = TRUE)

  expect_silent(embed_resources_streaming(layout$html_path))

  out <- paste(readLines(layout$html_path), collapse = "\n")
  expect_match(out, "report_files/missing.css", fixed = TRUE)
  expect_match(out, "report_files/missing.js",  fixed = TRUE)
})

test_that("embed_resources_streaming inlines <img> to data URIs", {
  skip_if_not_installed("xfun")

  png_magic <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
                        0x00, 0x00, 0x00, 0x0D))
  layout <- make_fake_report(
    html_lines = c(
      "<!doctype html><html><body>",
      '<img src="report_files/logo.png" alt="x" />',
      "</body></html>"
    ),
    files_layout = list("logo.png" = png_magic)
  )
  on.exit(unlink(layout$tmp_dir, recursive = TRUE), add = TRUE)

  embed_resources_streaming(layout$html_path)

  out <- paste(readLines(layout$html_path), collapse = "\n")
  expect_match(out, 'src="data:image/png;base64,')
  expect_false(grepl("report_files/logo.png", out, fixed = TRUE))
})

test_that("embed_resources_streaming rewrites url() refs inside CSS", {
  skip_if_not_installed("xfun")

  woff2_bytes <- charToRaw("fake-woff2-payload")
  layout <- make_fake_report(
    html_lines = c(
      "<!doctype html><html><head>",
      '<link rel="stylesheet" href="report_files/main.css" />',
      "</head></html>"
    ),
    files_layout = list(
      "main.css" = "@font-face { src: url(font.woff2) format('woff2'); }",
      "font.woff2" = woff2_bytes
    )
  )
  on.exit(unlink(layout$tmp_dir, recursive = TRUE), add = TRUE)

  embed_resources_streaming(layout$html_path)

  out <- paste(readLines(layout$html_path), collapse = "\n")
  expect_match(out, "url\\(data:[^)]*;base64,")
  expect_false(grepl("font.woff2", out, fixed = TRUE))
})

test_that("embed_resources_streaming preserves non-src attrs on <script>", {
  skip_if_not_installed("xfun")

  layout <- make_fake_report(
    html_lines = c(
      "<!doctype html><html><head>",
      '<script defer src="report_files/app.js" type="module"></script>',
      "</head></html>"
    ),
    files_layout = list("app.js" = "var x = 1;")
  )
  on.exit(unlink(layout$tmp_dir, recursive = TRUE), add = TRUE)

  embed_resources_streaming(layout$html_path)

  out <- paste(readLines(layout$html_path), collapse = "\n")
  expect_match(out, "<script\\s+defer\\s+type=\"module\">var x = 1;\\s*</script>")
})

test_that("embed_resources_streaming keeps sidecar when delete_files_dir = FALSE", {
  skip_if_not_installed("xfun")

  layout <- make_fake_report(
    html_lines = c(
      "<!doctype html><html><head>",
      '<script src="report_files/app.js"></script>',
      "</head></html>"
    ),
    files_layout = list("app.js" = "var x = 1;")
  )
  on.exit(unlink(layout$tmp_dir, recursive = TRUE), add = TRUE)

  embed_resources_streaming(layout$html_path, delete_files_dir = FALSE)
  expect_true(dir.exists(layout$files_dir))
})

test_that("embed_resources_streaming errors clearly on missing input", {
  expect_error(
    embed_resources_streaming(file.path(tempdir(), "no_such_file.html")),
    "HTML file not found"
  )
})
