# Tests for R/figures_export.R shared helpers (figures_dir, save_figure,
# save_figure_list). These cover the path-construction contract and the
# dual PDF + HTML write so the advanced_plots integration tests below
# can assume the helpers work.

test_that("figures_dir creates the module subfolder and validates inputs", {
  tmp <- withr::local_tempdir()
  out <- MStargetR:::figures_dir(tmp, "qcCheckR")
  expect_true(dir.exists(out))
  expect_equal(normalizePath(out, winslash = "/"),
               normalizePath(file.path(tmp, "all", "figures", "qcCheckR"),
                             winslash = "/"))

  expect_error(MStargetR:::figures_dir(tmp, "not_a_module"),
               "module")
  expect_error(MStargetR:::figures_dir(NULL, "qcCheckR"),
               "project_dir")
})

test_that("save_figure writes both PDF and HTML for a ggplot+plotly pair", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")
  tmp <- withr::local_tempdir()

  gg <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()
  pl <- plotly::ggplotly(gg)

  stem <- MStargetR:::save_figure(list(static = gg, interactive = pl),
                                  name = "scatter",
                                  project_dir = tmp,
                                  module = "qcCheckR")
  expect_true(file.exists(paste0(stem, ".pdf")))
  expect_true(file.exists(paste0(stem, ".html")))
  expect_gt(file.size(paste0(stem, ".pdf")), 100L)
  expect_gt(file.size(paste0(stem, ".html")), 500L)
})

test_that("save_figure converts a bare ggplot to interactive HTML via ggplotly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  tmp <- withr::local_tempdir()
  gg <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()
  stem <- MStargetR:::save_figure(gg, "ggonly", tmp, "batch_corrector")
  expect_true(file.exists(paste0(stem, ".pdf")))
  expect_true(file.exists(paste0(stem, ".html")))
})

test_that("save_figure rejects unsupported plot classes", {
  tmp <- withr::local_tempdir()
  expect_error(
    MStargetR:::save_figure("just a string", "x", tmp, "qcCheckR"),
    "must be a ggplot"
  )
})

test_that("save_figure_list iterates and skips NULL entries silently", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  tmp <- withr::local_tempdir()
  gg <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()
  plots <- list(a = gg, b = NULL, c = gg)
  stems <- MStargetR:::save_figure_list(plots, tmp, "results_explorer")
  expect_length(stems, 2L)
  expect_true(all(file.exists(paste0(stems, ".pdf"))))
  expect_true(all(file.exists(paste0(stems, ".html"))))
  expect_false(file.exists(file.path(tmp, "all", "figures",
                                     "results_explorer", "b.pdf")))
})

test_that("save_figure_list rejects unnamed lists", {
  tmp <- withr::local_tempdir()
  expect_error(
    MStargetR:::save_figure_list(list(NULL, NULL), tmp, "qcCheckR"),
    "named list"
  )
})
