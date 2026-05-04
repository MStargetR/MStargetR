# MStargetR Shiny Application - UI Definition
# ==============================================================================
# Premium cross-platform GUI using bslib (Bootstrap 5)

# -- Theme definition ----------------------------------------------------------
mst_theme <- bslib::bs_theme(
  version   = 5,
  preset    = "flatly",
  primary   = "#1e40af",
  secondary = "#6c757d",
  success   = "#059669",
  info      = "#0284c7",
  warning   = "#d97706",
  danger    = "#e11d48",
  base_font = tryCatch(bslib::font_google("Inter", local = TRUE),
                       error = function(e) "system-ui, -apple-system, sans-serif"),
  code_font = tryCatch(bslib::font_google("JetBrains Mono", local = TRUE),
                       error = function(e) "SFMono-Regular, Menlo, Consolas, monospace"),
  "navbar-bg"        = "#ffffff",
  "body-bg"          = "#f8f9fa",
  "card-border-color" = "#dee2e6"
)

# -- Helper to create a workflow card ------------------------------------------
workflow_card <- function(id, icon_text, title, description, step_number = NULL) {
  htmltools::tags$div(
    class = "col-sm-6 col-lg-4 col-xl-3 mb-4",
    htmltools::tags$div(
      class = "card workflow-card h-100",
      id = id,
      htmltools::tags$div(
        class = "card-body text-center position-relative",
        # Step number badge
        if (!is.null(step_number)) {
          htmltools::tags$span(
            class = "step-badge",
            step_number
          )
        },
        htmltools::tags$div(class = "workflow-icon", icon_text),
        htmltools::tags$h5(class = "card-title", title),
        htmltools::tags$p(class = "card-text", description),
        htmltools::tags$div(
          class = "workflow-arrow text-muted mt-2",
          shiny::icon("arrow-right")
        )
      )
    )
  )
}

# -- Helper to create a sidebar header ----------------------------------------
sidebar_header <- function(title, icon_name, description) {
  htmltools::tagList(
    htmltools::tags$div(
      class = "d-flex align-items-center gap-2 mb-2",
      htmltools::tags$span(
        class = "sidebar-accent-icon",
        shiny::icon(icon_name)
      ),
      htmltools::tags$span(class = "fw-semibold", title)
    ),
    htmltools::tags$p(class = "help-text mb-3", description)
  )
}

# -- Helper for collapsible example data structure ----------------------------
example_data_panel <- function(id, title, description, columns, rows,
                               notes = NULL) {
  # columns: named list of column descriptions, e.g. list("Col" = "desc")
  # rows: list of character vectors (one per row of example data)
  col_names <- names(columns)
  header_cells <- lapply(col_names, function(cn) {
    htmltools::tags$th(
      style = "white-space: nowrap; font-size: 0.75rem; padding: 4px 8px;",
      cn
    )
  })
  body_rows <- lapply(rows, function(row_vals) {
    cells <- lapply(row_vals, function(v) {
      htmltools::tags$td(
        style = "font-size: 0.75rem; padding: 3px 8px; font-family: 'JetBrains Mono', monospace;",
        v
      )
    })
    htmltools::tags$tr(cells)
  })
  col_desc_rows <- lapply(seq_along(columns), function(i) {
    htmltools::tags$tr(
      htmltools::tags$td(
        style = "font-weight: 600; font-size: 0.75rem; padding: 2px 6px;",
        col_names[i]
      ),
      htmltools::tags$td(
        style = "font-size: 0.75rem; padding: 2px 6px; color: #64748b;",
        columns[[i]]
      )
    )
  })

  collapse_id <- paste0("collapse_", id)

  htmltools::tags$div(
    class = "mt-3 mb-2",
    htmltools::tags$a(
      class = "text-decoration-none d-inline-flex align-items-center gap-1",
      style = "font-size: 0.82rem; cursor: pointer; color: #0284c7;",
      `data-bs-toggle` = "collapse",
      href = paste0("#", collapse_id),
      role = "button",
      `aria-expanded` = "false",
      shiny::icon("table-columns", class = "me-1"),
      title,
      shiny::icon("chevron-down", class = "ms-1",
                   style = "font-size: 0.65rem;")
    ),
    htmltools::tags$div(
      class = "collapse mt-2",
      id = collapse_id,
      htmltools::tags$div(
        class = "card",
        style = "border: 1px solid #e2e8f0; background: #f8fafc;",
        htmltools::tags$div(
          class = "card-body py-2 px-3",
          if (!is.null(description)) {
            htmltools::tags$p(
              class = "text-muted mb-2",
              style = "font-size: 0.78rem;",
              description
            )
          },
          # Example data table
          htmltools::tags$div(
            class = "mb-2",
            htmltools::tags$small(class = "fw-semibold text-uppercase",
                                  style = "letter-spacing: 0.04em; color: #475569;",
                                  "Example rows"),
            htmltools::tags$div(
              style = "overflow-x: auto;",
              htmltools::tags$table(
                class = "table table-sm table-bordered mb-2",
                style = "font-size: 0.75rem; margin-bottom: 0;",
                htmltools::tags$thead(
                  htmltools::tags$tr(
                    style = "background: #e2e8f0;",
                    header_cells
                  )
                ),
                htmltools::tags$tbody(body_rows)
              )
            )
          ),
          # Column descriptions
          htmltools::tags$details(
            class = "mb-1",
            htmltools::tags$summary(
              style = "font-size: 0.75rem; cursor: pointer; color: #64748b;",
              "Column descriptions"
            ),
            htmltools::tags$table(
              class = "table table-sm mb-0 mt-1",
              style = "font-size: 0.75rem;",
              htmltools::tags$tbody(col_desc_rows)
            )
          ),
          if (!is.null(notes)) {
            htmltools::tags$div(
              class = "mt-2 pt-1 border-top",
              style = "font-size: 0.73rem; color: #64748b;",
              shiny::icon("circle-info", class = "me-1"),
              notes
            )
          }
        )
      )
    )
  )
}

# -- Helper for empty-state placeholder ---------------------------------------
# output_id: when supplied, the placeholder is shown only while that output has
# not yet rendered (conditionalPanel hides it once the plot/table is available).
empty_state <- function(message = "Run the pipeline to see results here.",
                        output_id = NULL) {
  inner <- htmltools::tags$div(
    class = "empty-state text-center py-5",
    htmltools::tags$div(
      class = "empty-state-icon mb-3",
      shiny::icon("flask", class = "fa-2x text-muted")
    ),
    htmltools::tags$p(class = "text-muted mb-0", message)
  )
  if (!is.null(output_id) && nzchar(output_id)) {
    shiny::conditionalPanel(
      condition = paste0("!output[\"", output_id, "\"]"),
      inner
    )
  } else {
    inner
  }
}

# == Main UI ===================================================================
ui <- bslib::page_navbar(
  lang = "en",
  title = htmltools::tags$span(
    htmltools::tags$strong("MStargetR")
  ),
  id = "main_nav",
  theme = mst_theme,
  header = htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
      htmltools::tags$link(rel = "stylesheet", href = "custom.css"),
      htmltools::tags$link(rel = "stylesheet", href = "premium-polish.css"),
      htmltools::tags$link(rel = "stylesheet", href = "a11y-fixes.css"),
      htmltools::tags$script(src = paste0("app.js?v=",
        tryCatch(utils::packageVersion("MStargetR"), error = function(e) "0"))),
      htmltools::tags$script(src = paste0("premium-interactions.js?v=",
        tryCatch(utils::packageVersion("MStargetR"), error = function(e) "0")))
    ),
    # Floating cancel button rendered by server when a process is running
    shiny::uiOutput("global_cancel_ui")
  ),
  footer = htmltools::tags$footer(
    class = "mst-footer text-center py-3 mt-4",
    htmltools::tags$small(
      class = "text-muted",
      "MStargetR | ",
      htmltools::tags$a(
        href = "https://github.com/MStargetR/MStargetR",
        target = "_blank",
        rel = "noopener noreferrer",
        "GitHub"
      ),
      " | MIT License"
    )
  ),
  # Theme toggle in the navbar
  bslib::nav_spacer(),
  bslib::nav_item(
    htmltools::tags$button(
      id = "theme_toggle",
      class = "btn",
      title = "Toggle dark/light mode",
      `aria-label` = "Toggle dark/light mode",
      "\u263E"
    )
  ),

  # ============================================================================
  # TAB 1: Dashboard
  # ============================================================================
  bslib::nav_panel(
    title = "Dashboard",
    icon  = shiny::icon("house"),
    htmltools::tags$div(
      class = "container-fluid py-4",

      # Welcome banner - hero section with gradient
      htmltools::tags$div(
        class = "row mb-4",
        htmltools::tags$div(
          class = "col-12",
          htmltools::tags$div(
            class = "hero-banner",
            htmltools::tags$div(
              class = "hero-banner-content",
              htmltools::tags$div(
                class = "d-flex align-items-center gap-3 flex-wrap",
                htmltools::tags$div(
                  style = "font-size:3.5rem; line-height:1;",
                  "\U0001F9EA"
                ),
                htmltools::tags$div(
                  htmltools::tags$h2(
                    class = "mb-1 fw-bold hero-title",
                    "Welcome to MStargetR"
                  ),
                  htmltools::tags$p(
                    class = "mb-2 hero-subtitle",
                    "Targeted MRM Mass Spectrometry preprocessing pipeline"
                  ),
                  htmltools::tags$p(
                    class = "mb-0 hero-description",
                    "Preprocessing targeted LC-MS mass spectrometry data. ",
                    "Follow the workflow below or jump to any step."
                  ),
                  htmltools::tags$span(
                    class = "version-pill mt-2",
                    paste0("MStargetR v",
                           utils::packageVersion("MStargetR"))
                  )
                )
              )
            )
          )
        )
      ),

      # Recent Projects quick-select
      htmltools::tags$div(
        class = "row mb-4",
        htmltools::tags$div(
          class = "col-md-6 col-lg-4",
          shiny::selectInput("quick_project", "Recent Projects",
                              choices = c("Select a recent project..." = ""),
                              width = "100%")
        )
      ),

      # Quick-start workflow cards
      htmltools::tags$h5(class = "section-heading", "Workflow"),
      htmltools::tags$div(
        class = "row",
        workflow_card(
          "dash_card_convert", "\U0001F504",
          "File Conversion",
          "Convert vendor files to mzML format via Docker / ProteoWizard",
          step_number = 1
        ),
        workflow_card(
          "dash_card_peak", "\U0001F4C8",
          "Peak Integration",
          "Peak picking and integration via Skyline in Docker",
          step_number = 2
        ),
        workflow_card(
          "dash_card_qc", "\u2705",
          "Quality Control",
          "QC checking, batch correction, filtering, and reporting",
          step_number = 3
        ),
        workflow_card(
          "dash_card_batch", "\U0001F9EA",
          "Batch Correction",
          "Standalone interbatch correction with multiple methods",
          step_number = 4
        ),
        workflow_card(
          "dash_card_results", "\U0001F4CA",
          "Results Explorer",
          "Interactive plots, heatmaps, and data tables of processed results",
          step_number = 5
        ),
        workflow_card(
          "dash_card_utils", "\U0001F527",
          "Utilities",
          "Transition checking, template comparison, dependency validation",
          step_number = 6
        ),
        workflow_card(
          "dash_card_settings", "\u2699\uFE0F",
          "Settings",
          "Configure user defaults, theme, and Docker settings",
          step_number = 7
        )
      ),

      # System status row
      htmltools::tags$h5(class = "section-heading mt-4", "System Status"),
      htmltools::tags$div(
        class = "row",
        htmltools::tags$div(
          class = "col-md-6 col-lg-3 mb-3",
          htmltools::tags$div(
            class = "value-box",
            htmltools::tags$div(
              class = "d-flex align-items-center gap-2 value-box-title",
              shiny::icon("docker", lib = "font-awesome", class = "text-info"),
              "Docker"
            ),
            shiny::uiOutput("dash_docker_status")
          )
        ),
        htmltools::tags$div(
          class = "col-md-6 col-lg-3 mb-3",
          htmltools::tags$div(
            class = "value-box",
            htmltools::tags$div(
              class = "d-flex align-items-center gap-2 value-box-title",
              shiny::icon("box-open", class = "text-primary"),
              "MStargetR"
            ),
            shiny::uiOutput("dash_package_status")
          )
        ),
        htmltools::tags$div(
          class = "col-md-6 col-lg-3 mb-3",
          htmltools::tags$div(
            class = "value-box",
            htmltools::tags$div(
              class = "d-flex align-items-center gap-2 value-box-title",
              shiny::icon("code", class = "text-success"),
              "R Version"
            ),
            htmltools::tags$div(
              class = "value-box-value",
              paste0("R ", R.version$major, ".", R.version$minor)
            )
          )
        ),
        htmltools::tags$div(
          class = "col-md-6 col-lg-3 mb-3",
          htmltools::tags$div(
            class = "value-box",
            htmltools::tags$div(
              class = "d-flex align-items-center gap-2 value-box-title",
              shiny::icon("desktop", class = "text-warning"),
              "Platform"
            ),
            htmltools::tags$div(
              class = "value-box-value",
              .Platform$OS.type
            )
          )
        )
      ),

      # Recent projects (detailed list)
      shiny::uiOutput("dash_recent_projects")
    )
  ),

  # ============================================================================
  # TAB 2: File Conversion (msConvertR)
  # ============================================================================
  bslib::nav_panel(
    title = "File Conversion",
    icon  = shiny::icon("file-export"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "msConvertR Parameters",
        width = 380,

        sidebar_header(
          "File Conversion",
          "file-export",
          "Convert vendor mass spec files (.wiff, .raw, .d) to open mzML format using Docker + msConvert. Place files in a 'raw_data' subfolder."
        ),

        htmltools::tags$label(class = "form-label", `for` = "convert_input_dir", "Input Directory"),
        htmltools::tags$div(
          class = "input-group mb-1",
          shiny::textInput("convert_input_dir", NULL,
                           placeholder = "Click 'Select Folder' to choose") |>
            htmltools::tagAppendAttributes(style = "flex:1;"),
          htmltools::tags$div(
            class = "input-group-append",
            style = "margin-bottom:15px;",
            shiny::actionButton(
              "convert_input_dir_browse",
              htmltools::tagList(shiny::icon("folder-open"), " Select Folder"),
              class = "btn btn-primary"
            )
          )
        ),
        shiny::uiOutput("convert_input_dir_status"),
        htmltools::tags$p(class = "help-text",
          "Directory containing raw vendor files (e.g., .wiff, .raw, .d)"
        ),

        htmltools::tags$label(class = "form-label", `for` = "convert_output_dir", "Output Directory"),
        htmltools::tags$div(
          class = "input-group mb-1",
          shiny::textInput("convert_output_dir", NULL,
                           placeholder = "Click 'Select Folder' to choose") |>
            htmltools::tagAppendAttributes(style = "flex:1;"),
          htmltools::tags$div(
            class = "input-group-append",
            style = "margin-bottom:15px;",
            shiny::actionButton(
              "convert_output_dir_browse",
              htmltools::tagList(shiny::icon("folder-open"), " Select Folder"),
              class = "btn btn-primary"
            )
          )
        ),
        shiny::uiOutput("convert_output_dir_status"),
        htmltools::tags$p(class = "help-text",
          "Where converted .mzML files will be saved"
        ),

        shiny::tags$div(
          class = "alert alert-info py-2 px-3 mb-2",
          style = "font-size:0.85em;",
          shiny::icon("circle-info", class = "me-1"),
          "Vendor format is auto-detected by ProteoWizard. No manual selection required."
        ),

        htmltools::tags$hr(),

        shiny::uiOutput("convert_docker_indicator"),

        htmltools::tags$hr(),

        shiny::actionButton(
          "convert_run", "Run Conversion",
          class = "btn-run btn-run-prominent w-100",
          icon = shiny::icon("play")
        )
      ),

      # Main panel
      htmltools::tags$div(
        class = "container-fluid py-3",

        htmltools::tags$div(
          class = "card mb-3",
          htmltools::tags$div(
            class = "card-header d-flex align-items-center gap-2",
            shiny::icon("terminal", class = "text-muted"),
            "Console Output"
          ),
          htmltools::tags$div(
            class = "card-body p-0",
            shiny::verbatimTextOutput("convert_console") |>
              htmltools::tagAppendAttributes(class = "console-output m-0")
          )
        ),

        htmltools::tags$div(
          class = "card",
          htmltools::tags$div(
            class = "card-header d-flex align-items-center gap-2",
            shiny::icon("folder-open", class = "text-muted"),
            "Output Files"
          ),
          htmltools::tags$div(
            class = "card-body",
            DT::DTOutput("convert_output_table")
          )
        )
      )
    )
  ),

  # ============================================================================
  # TAB 3: Peak Integration (PeakForgeR)
  # ============================================================================
  bslib::nav_panel(
    title = "Peak Integration",
    icon  = shiny::icon("chart-line"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "PeakForgeR Parameters",
        width = 380,

        sidebar_header(
          "Peak Integration",
          "chart-line",
          "Integrate chromatographic peaks using Skyline via Docker. Upload your MRM template TSV file(s) defining target transitions."
        ),

        shiny::textInput(
          "peak_user_name", "User Name",
          placeholder = "Enter your username"
        ),

        htmltools::tags$label(class = "form-label", `for` = "peak_project_dir", "Project Directory"),
        htmltools::tags$div(
          class = "input-group mb-1",
          shiny::textInput("peak_project_dir", NULL,
                           placeholder = "Click 'Select Folder' to choose") |>
            htmltools::tagAppendAttributes(style = "flex:1;"),
          htmltools::tags$div(
            class = "input-group-append",
            style = "margin-bottom:15px;",
            shiny::actionButton(
              "peak_project_dir_browse",
              htmltools::tagList(shiny::icon("folder-open"), " Select Folder"),
              class = "btn btn-primary"
            )
          )
        ),
        shiny::uiOutput("peak_project_dir_status"),
        htmltools::tags$p(class = "help-text",
          "Root project directory created by msConvertR or containing mzML files"
        ),

        htmltools::tags$hr(),
        htmltools::tags$h6(class = "fw-semibold", "MRM Template File"),
        htmltools::tags$p(class = "help-text",
          "Upload MRM transition list file (.tsv/.csv)"
        ),
        htmltools::tags$div(
          class = "file-upload-area",
          shiny::fileInput(
            "peak_mrm_templates", "MRM Template",
            multiple = TRUE,
            accept = c(".tsv", ".csv", ".txt")
          ),
          htmltools::tags$p(class = "help-text text-center mt-n2",
                             "Drag & drop files here or click to browse")
        ),

        example_data_panel(
          id = "peak_mrm_example",
          title = "Expected MRM template format",
          description = "Tab-separated file defining MRM transitions. Use SIL_ prefix for internal standards.",
          columns = list(
            "Molecule List Name" = "Lipid class (e.g., CE, PC, PE, SM)",
            "Precursor Name"     = "Molecule name; prefix with SIL_ for internal standards",
            "Precursor Mz"       = "Precursor m/z value",
            "Precursor Charge"   = "Precursor charge state (usually 1)",
            "Product Mz"         = "Product (fragment) m/z value",
            "Product Charge"     = "Product charge state (usually 1)",
            "Explicit Retention Time" = "Expected retention time in minutes",
            "Explicit Retention Time Window" = "RT window tolerance in minutes",
            "Note"               = "Reference SIL name for quantification (NA for SILs)",
            "control_chart"      = "Include in QC control charts (TRUE/FALSE)"
          ),
          rows = list(
            c("CE", "CE(14:0)", "614.6", "1", "369.4", "1", "11.6", "0.5", "SIL_CE(16:0)_d7", "FALSE"),
            c("CE", "CE(16:0)", "642.6", "1", "369.4", "1", "12.3", "0.5", "SIL_CE(16:0)_d7", "TRUE"),
            c("CE", "SIL_CE(16:0)_d7", "649.6", "1", "376.4", "1", "12.3", "0.5", "NA", "TRUE")
          ),
          notes = "SIL rows must have a matching Precursor Name used in the Note column of analyte rows."
        ),

        htmltools::tags$hr(),

        htmltools::tags$h6(class = "fw-semibold", "QC Sample Label"),
        htmltools::tags$p(class = "help-text",
          "Tag to identify QC samples in file names (case-insensitive)"
        ),
        shiny::textInput(
          "peak_qc_label", NULL,
          value = "LTR",
          placeholder = "e.g., LTR, QC, POOLED"
        ),

        htmltools::tags$p(class = "help-text",
          "Plate IDs are auto-detected from the project directory structure."
        ),

        htmltools::tags$hr(),

        shiny::actionButton(
          "peak_run", "Run Peak Integration",
          class = "btn-run btn-run-prominent w-100",
          icon = shiny::icon("play")
        )
      ),

      # Main panel
      htmltools::tags$div(
        class = "container-fluid py-3",

        # MRM template preview (compact)
        htmltools::tags$div(
          class = "card mb-3",
          htmltools::tags$div(
            class = "card-header d-flex align-items-center gap-2",
            shiny::icon("table", class = "text-muted"),
            "MRM Template Preview"
          ),
          htmltools::tags$div(
            class = "card-body p-2",
            style = "max-height: 300px; overflow-y: auto;",
            DT::DTOutput("peak_mrm_preview")
          ),
          # Auto transition check result
          shiny::uiOutput("peak_transition_result"),
          shiny::conditionalPanel(
            condition = "output.peak_transition_has_dupes",
            htmltools::tags$div(
              class = "card-body p-2 border-top",
              htmltools::tags$h6(class = "text-danger fw-semibold mb-2",
                "Duplicate Transitions"),
              DT::DTOutput("peak_transition_table")
            )
          )
        ),

        htmltools::tags$div(
          class = "card mb-3",
          htmltools::tags$div(
            class = "card-header d-flex align-items-center gap-2",
            shiny::icon("terminal", class = "text-muted"),
            "Console Output"
          ),
          htmltools::tags$div(
            class = "card-body p-0",
            shiny::verbatimTextOutput("peak_console") |>
              htmltools::tagAppendAttributes(class = "console-output m-0")
          )
        ),

        htmltools::tags$div(
          class = "card",
          htmltools::tags$div(
            class = "card-header d-flex align-items-center gap-2",
            shiny::icon("table-list", class = "text-muted"),
            "PeakForgeR Results"
          ),
          htmltools::tags$div(
            class = "card-body",
            shiny::uiOutput("peak_results_tabs"),
            empty_state("Run Peak Integration to see results here.", "peak_results_tabs")
          )
        )
      )
    )
  ),

  # ============================================================================
  # TAB 4: Quality Control (qcCheckR)
  # ============================================================================
  bslib::nav_panel(
    title = "Quality Control",
    icon  = shiny::icon("clipboard-check"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "qcCheckR Parameters",
        width = 380,

        sidebar_header(
          "Quality Control",
          "clipboard-check",
          "Run quality control: sample classification, batch correction, metabolite filtering, and reporting. Requires SIL guide + concentration guide files."
        ),

        shiny::textInput(
          "qc_user_name", "User Name",
          placeholder = "Enter your username"
        ),

        htmltools::tags$label(class = "form-label", `for` = "qc_project_dir", "Project Directory"),
        htmltools::tags$div(
          class = "input-group mb-1",
          shiny::textInput("qc_project_dir", NULL,
                           placeholder = "Click 'Select Folder' to choose") |>
            htmltools::tagAppendAttributes(style = "flex:1;"),
          htmltools::tags$div(
            class = "input-group-append",
            style = "margin-bottom:15px;",
            shiny::actionButton(
              "qc_project_dir_browse",
              htmltools::tagList(shiny::icon("folder-open"), " Select Folder"),
              class = "btn btn-primary"
            )
          )
        ),
        shiny::uiOutput("qc_project_dir_status"),

        htmltools::tags$hr(),
        htmltools::tags$h6(class = "fw-semibold", "MRM Template List"),
        htmltools::tags$p(class = "help-text",
          "Upload paired SIL guide and concentration guide files for each method version."
        ),

        htmltools::tags$div(
          class = "file-upload-area",
          shiny::fileInput(
            "qc_mrm_file", "SIL Guide File",
            accept = c(".tsv", ".csv", ".txt")
          ),
          htmltools::tags$p(class = "help-text text-center mt-n2",
                             "Drag & drop file here or click to browse")
        ),

        example_data_panel(
          id = "qc_sil_example",
          title = "Expected SIL guide format",
          description = "Same format as the MRM template used in Peak Integration.",
          columns = list(
            "Molecule List Name" = "Lipid class (e.g., CE, PC, SM)",
            "Precursor Name"     = "Molecule name (SIL_ prefix for standards)",
            "Precursor Mz"       = "Precursor m/z",
            "Precursor Charge"   = "Charge state",
            "Product Mz"         = "Product m/z",
            "Product Charge"     = "Product charge",
            "Explicit Retention Time" = "RT in minutes",
            "Explicit Retention Time Window" = "RT window",
            "Note"               = "Linked SIL name (NA for SIL rows)",
            "control_chart"      = "Include in control charts"
          ),
          rows = list(
            c("CE", "CE(16:0)", "642.6", "1", "369.4", "1", "12.3", "0.5", "SIL_CE(16:0)_d7", "TRUE"),
            c("CE", "SIL_CE(16:0)_d7", "649.6", "1", "376.4", "1", "12.3", "0.5", "NA", "TRUE")
          )
        ),

        htmltools::tags$div(
          class = "file-upload-area",
          shiny::fileInput(
            "qc_conc_file", "Concentration Guide File",
            accept = c(".tsv", ".csv", ".txt")
          ),
          htmltools::tags$p(class = "help-text text-center mt-n2",
                             "Drag & drop file here or click to browse")
        ),

        example_data_panel(
          id = "qc_conc_example",
          title = "Expected concentration guide format",
          description = "Maps internal standards (SILs) to their concentrations for quantification.",
          columns = list(
            "SIL_source"            = "Vendor/source of the standard mix",
            "ISTD"                  = "Internal standard name",
            "MW"                    = "Molecular weight (g/mol)",
            "ug/mL"                 = "Stock concentration in ug/mL",
            "uM"                    = "Stock concentration in uM",
            "concentration_factor"  = "Final correction factor applied to concentrations",
            "SIL_name"              = "Must match Precursor Name in SIL guide"
          ),
          rows = list(
            c("UltimateSPLASH", "SIL_14:1 cholesteryl-d7 ester", "601.58", "25", "41.56", "0.75", "SIL_14:1 cholesteryl-d7 ester"),
            c("UltimateSPLASH", "SIL_16:1 cholesteryl-d7 ester", "629.61", "50", "79.41", "1.43", "SIL_16:1 cholesteryl-d7 ester")
          ),
          notes = "SIL_name must exactly match the Precursor Name in the SIL guide file."
        ),

        # Dynamic template builder (shows added pairs)
        shiny::uiOutput("qc_template_builder"),

        # Auto compare result (populated when templates are added)
        shiny::uiOutput("qc_compare_result"),

        shiny::actionButton(
          "qc_add_version", "Add Method Version",
          class = "btn btn-outline-secondary btn-sm mt-2",
          icon = shiny::icon("plus")
        ),

        htmltools::tags$hr(),

        htmltools::tags$h6(class = "fw-semibold", "QC Sample Label"),
        htmltools::tags$p(class = "help-text",
          "The tag that identifies your QC samples in file names. ",
          "Must also appear in the Sample Tags list below. Case-insensitive."
        ),
        shiny::textInput(
          "qc_qc_label", NULL,
          value = "LTR",
          placeholder = "e.g., LTR, PQC, QC"
        ),

        shiny::textInput(
          "qc_sample_tags", "Sample Tags",
          value = "sample,control,blank,qc",
          placeholder = "Comma-separated tags"
        ),
        shiny::actionButton(
          "qc_use_anpc_defaults", "Use ANPC defaults",
          class = "btn btn-outline-secondary btn-sm",
          icon = shiny::icon("rotate-left")
        ),
        htmltools::tags$p(class = "help-text",
          "Comma-separated list of ALL tags that appear in your sample file names. ",
          "Your QC label is added automatically if missing. ",
          "Example: sample,blank,pqc,ltr. ",
          "The 'Use ANPC defaults' button fills in the standard ANPC tag set ",
          "(pqc, qc, vltr, sltr, ltr, blank, istds, cond, sample)."
        ),

        shiny::sliderInput(
          "qc_mv_threshold", "Missing Value Threshold (%)",
          min = 0, max = 100, value = 50, step = 5
        ),
        htmltools::tags$p(class = "help-text",
          "Metabolites with more missing values than this threshold are filtered"
        ),

        htmltools::tags$hr(),
        htmltools::tags$h6(class = "fw-semibold", "Batch Correction Options"),

        shiny::selectInput(
          "qc_batch_method", "Batch Correction Method",
          choices  = c(
            "QCRFSC (Random Forest)" = "QCRFSC",
            "ComBat (Empirical Bayes, QC-free)" = "ComBat"
          ),
          selected = "QCRFSC"
        ),
        htmltools::tags$p(class = "help-text",
          "QCRFSC = QC-based Random Forest Signal Correction, ComBat = Empirical Bayes (no QC required)"
        ),

        shiny::conditionalPanel(
          condition = "input.qc_batch_method == 'QCRFSC'",
          shiny::sliderInput(
            "qc_batch_ntree", "Number of Trees (ntree)",
            min = 100, max = 20000, value = 500, step = 100
          ),
          htmltools::tags$p(class = "help-text",
            "Number of trees in the random forest model. Higher values give more stable corrections but take longer."
          )
        ),

        shiny::conditionalPanel(
          condition = "input.qc_batch_method == 'ComBat'",
          shiny::checkboxInput("qc_combat_par_prior", "Parametric priors", value = TRUE),
          shiny::checkboxInput("qc_combat_mean_only", "Mean-only correction", value = FALSE),
          shiny::textInput("qc_combat_ref_batch", "Reference batch (optional)", value = "")
        ),

        shiny::sliderInput(
          "qc_batch_coCV", "Coefficient of Variation (%)",
          min = 0, max = 100, value = 30, step = 5
        ),
        htmltools::tags$p(class = "help-text",
          "Maximum QC coefficient of variation allowed. Metabolites with QC CV above this threshold are flagged."
        ),

        shiny::sliderInput(
          "qc_batch_Frule", "80% Rule Filter (%)",
          min = 0, max = 100, value = 80, step = 5
        ),
        htmltools::tags$p(class = "help-text",
          "Minimum percentage of non-missing values required per metabolite. Set to 0% to disable."
        ),

        shiny::selectInput(
          "qc_batch_imputeM", "Imputation Method",
          choices  = c("minHalf", "median", "mean", "knn"),
          selected = "minHalf"
        ),

        htmltools::tags$hr(),

        shiny::actionButton(
          "qc_run", "Run QC Check",
          class = "btn-run btn-run-prominent w-100",
          icon = shiny::icon("play")
        )
      ),

      # Main panel
      htmltools::tags$div(
        class = "container-fluid py-3",

        htmltools::tags$div(
          class = "card mb-3",
          htmltools::tags$div(
            class = "card-header d-flex align-items-center gap-2",
            shiny::icon("terminal", class = "text-muted"),
            "Console Output"
          ),
          htmltools::tags$div(
            class = "card-body p-0",
            shiny::verbatimTextOutput("qc_console") |>
              htmltools::tagAppendAttributes(class = "console-output m-0")
          )
        ),

        # RDA background-save indicator. The qcCheckR pipeline returns the
        # in-memory result as soon as compute finishes; the slow compressed
        # RDA write happens in a separate detached background subprocess so
        # users can interact with results immediately. This banner is only
        # rendered while that detached save is still running.
        shiny::uiOutput("qc_rda_save_status"),

        # Results in sub-tabs
        bslib::navset_card_tab(
          id = "qc_results_tabs",
          title = "QC Results",

          bslib::nav_panel(
            title = htmltools::tagList(shiny::icon("table-cells"), "Summary"),
            htmltools::tags$div(
              class = "p-3",
              DT::DTOutput("qc_summary_table"),
              empty_state("Run QC Check to see the summary here.", "qc_summary_table")
            )
          ),
          bslib::nav_panel(
            title = htmltools::tagList(shiny::icon("chart-line"), "PCA Plot"),
            htmltools::tags$div(
              class = "p-3",
              plotly::plotlyOutput("qc_pca_plot", height = "700px"),
              empty_state("Run QC Check to see the PCA plot here.", "qc_pca_plot")
            )
          ),
          bslib::nav_panel(
            title = htmltools::tagList(shiny::icon("arrow-down-1-9"), "Run Order"),
            htmltools::tags$div(
              class = "p-3",
              shiny::selectInput(
                "qc_runorder_pc", "Select Principal Component",
                choices = c("PC1" = "PC1", "PC2" = "PC2", "PC3" = "PC3"),
                selected = "PC1"
              ),
              plotly::plotlyOutput("qc_runorder_plot", height = "550px"),
              empty_state("Run QC Check to see run order plots here.", "qc_runorder_plot")
            )
          ),
          bslib::nav_panel(
            title = htmltools::tagList(shiny::icon("chart-bar"), "Control Charts"),
            htmltools::tags$div(
              class = "p-3",
              shiny::selectInput(
                "qc_controlchart_metabolite", "Select Metabolite",
                choices = NULL
              ),
              plotly::plotlyOutput("qc_controlchart_plot", height = "550px"),
              empty_state("Run QC Check to see control charts here.", "qc_controlchart_plot")
            )
          ),
          bslib::nav_panel(
            title = htmltools::tagList(shiny::icon("chart-pie"), "RSD Distribution"),
            htmltools::tags$div(
              class = "p-3",
              plotly::plotlyOutput("qc_rsd_histogram", height = "550px"),
              empty_state("Run QC Check to see the RSD distribution.", "qc_rsd_histogram")
            )
          ),
          bslib::nav_panel(
            title = htmltools::tagList(shiny::icon("eye-slash"), "Missing Values"),
            htmltools::tags$div(
              class = "p-3",
              plotly::plotlyOutput("qc_missing_plot", height = "550px"),
              empty_state("Run QC Check to see missing value patterns.", "qc_missing_plot")
            )
          ),
          bslib::nav_panel(
            title = htmltools::tagList(shiny::icon("users"), "Sample Overview"),
            htmltools::tags$div(
              class = "p-3",
              htmltools::tags$div(
                class = "row mb-3",
                htmltools::tags$div(
                  class = "col-md-6",
                  plotly::plotlyOutput("qc_sample_type_pie", height = "450px")
                ),
                htmltools::tags$div(
                  class = "col-md-6",
                  plotly::plotlyOutput("qc_plate_bar", height = "450px")
                )
              ),
              empty_state("Run QC Check to see sample overview.", "qc_plate_bar")
            )
          ),
          bslib::nav_panel(
            title = htmltools::tagList(shiny::icon("filter"), "Filtered Metabolites"),
            htmltools::tags$div(
              class = "p-3",
              DT::DTOutput("qc_filtered_table"),
              empty_state("Run QC Check to see filtered metabolites here.", "qc_filtered_table")
            )
          )
        ),

        # Export buttons
        htmltools::tags$div(
          class = "d-flex gap-3 mt-3",
          shiny::downloadButton("qc_download_excel", "Export Excel",
                                class = "btn btn-outline-secondary"),
          shiny::downloadButton("qc_download_report", "Export HTML Report",
                                class = "btn btn-outline-secondary")
        )
      )
    )
  ),

  # ============================================================================
  # TAB 5: Batch Correction (batchCorrectR)
  # ============================================================================
  bslib::nav_panel(
    title = "Batch Correction",
    icon  = shiny::icon("sliders"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "batchCorrectR Parameters",
        width = 380,

        sidebar_header(
          "Batch Correction",
          "sliders",
          "Standalone interbatch correction. Upload a data file or use qcCheckR output. Supports QC-based (QCRFSC) and QC-free (ComBat) methods."
        ),

        shiny::radioButtons(
          "batch_data_source", "Data Source",
          choices = c(
            "Upload file"    = "upload",
            "Use qcCheckR output" = "pipeline"
          ),
          selected = "upload"
        ),

        shiny::conditionalPanel(
          condition = "input.batch_data_source == 'upload'",
          htmltools::tags$div(
            class = "file-upload-area",
            shiny::fileInput(
              "batch_data_upload", "Upload Data File(s)",
              accept = c(".csv", ".tsv", ".txt", ".xlsx"),
              multiple = TRUE
            ),
            htmltools::tags$p(class = "help-text text-center mt-n2",
                               "Drag & drop one or more files (.csv, .tsv, .xlsx). ",
                               "Multiple files are joined on matching columns.")
          ),
          # Sheet selector appears when an .xlsx file is uploaded
          shiny::uiOutput("batch_xlsx_sheet_selector"),

          example_data_panel(
            id = "batch_upload_example",
            title = "Expected data format",
            description = "Rows = samples, columns = metadata + metabolites. All numeric columns not listed as metadata are treated as metabolite features.",
            columns = list(
              "sample_name"         = "Unique sample identifier (required)",
              "sample_plate_id"     = "Batch/plate ID (required); alternative: batch",
              "sample_type_factor"  = "Sample type: qc, sample, blank, etc. (required); alternative: sample_type",
              "sample_run_index"    = "Injection order as integer (required); alternative: run_order",
              "[metabolite cols]"   = "Numeric concentration or peak area values (one column per metabolite)"
            ),
            rows = list(
              c("PLASMA_LTR_01", "PLATE_1", "qc",     "1", "382663.0"),
              c("Sample_001",    "PLATE_1", "sample", "2", "132454.2"),
              c("Sample_002",    "PLATE_1", "sample", "3", "198712.8"),
              c("PLASMA_LTR_02", "PLATE_2", "qc",     "4", "375891.5")
            ),
            notes = "QCRFSC requires at least 2 QC samples per batch. ComBat does not require QC samples."
          )
        ),

        shiny::conditionalPanel(
          condition = "input.batch_data_source == 'pipeline'",
          htmltools::tags$div(
            class = "alert alert-info py-2 px-3",
            style = "font-size: 0.82rem;",
            shiny::icon("info-circle"),
            " Data will be loaded from the qcCheckR corrected concentration data. ",
            "Corrected output will be saved to ",
            htmltools::tags$code("project/all/batch_correction/"),
            " (qcCheckR statTarget output is preserved)."
          ),
          shiny::uiOutput("batch_pipeline_source_info")
        ),

        shiny::textInput(
          "batch_qc_label", "QC Sample Label",
          value = "LTR",
          placeholder = "e.g., LTR, PQC, QC"
        ),
        htmltools::tags$p(class = "help-text",
          "Tag that identifies your QC samples (matched against sample_type or sample_type_factor). ",
          "Required for QCRFSC. Not needed for ComBat."
        ),

        shiny::textInput(
          "batch_sample_tags", "Sample Tags",
          value = "sample,control,blank,qc",
          placeholder = "Comma-separated tags"
        ),
        htmltools::tags$p(class = "help-text",
          "Comma-separated list of sample_type values to KEEP for correction. ",
          "Rows whose sample_type is not in this list (and does not match the QC label) are excluded ",
          "before the statTarget model is fit -- use this to drop blanks / double-blanks / ",
          "other low-signal types. Leave blank to keep every row."
        ),

        htmltools::tags$hr(),

        shiny::selectInput(
          "batch_method", "Correction Method",
          choices = c(
            "QCRFSC (Random Forest)" = "QCRFSC",
            "ComBat (Empirical Bayes, QC-free)" = "ComBat"
          ),
          selected = "QCRFSC"
        ),
        htmltools::tags$p(class = "help-text",
          htmltools::tags$strong("QCRFSC:"),
          " QC-based Random Forest Signal Correction. Uses QC samples to model and remove signal drift within and between batches. Requires QC samples in each batch.",
          htmltools::tags$br(),
          htmltools::tags$strong("ComBat:"),
          " Empirical Bayes batch correction (Johnson et al. 2007). Does not require QC samples. Adjusts for systematic batch differences using parametric or non-parametric priors."
        ),

        shiny::conditionalPanel(
          condition = "input.batch_method == 'QCRFSC'",
          shiny::sliderInput(
            "batch_ntree", "Number of Trees (ntree)",
            min = 100, max = 20000, value = 500, step = 100
          ),
          htmltools::tags$p(class = "help-text",
            "Number of trees in the random forest model. Higher values give more stable corrections but take longer."
          )
        ),

        shiny::conditionalPanel(
          condition = "input.batch_method == 'ComBat'",
          htmltools::tags$hr(),
          htmltools::tags$h6("ComBat Options"),
          shiny::checkboxInput("combat_par_prior", "Parametric priors", value = TRUE),
          htmltools::tags$p(class = "help-text",
            "Use parametric (normal/inverse-gamma) prior distributions. Uncheck for non-parametric estimation (slower but more flexible for non-normal data)."
          ),
          shiny::checkboxInput("combat_mean_only", "Mean-only correction", value = FALSE),
          htmltools::tags$p(class = "help-text",
            "Only adjust batch means, not variances. Use when batch effects primarily shift the mean signal level."
          ),
          shiny::textInput("combat_ref_batch", "Reference batch (optional)", value = ""),
          htmltools::tags$p(class = "help-text",
            "If set, other batches are adjusted to match this reference batch. Leave blank to use the grand mean."
          )
        ),

        shiny::sliderInput(
          "batch_coCV", "Coefficient of Variation (%)",
          min = 0, max = 100, value = 30, step = 5
        ),
        htmltools::tags$p(class = "help-text",
          "Maximum QC coefficient of variation allowed. Metabolites with QC CV above this threshold are flagged. Set high (e.g., 100%) to disable filtering."
        ),

        shiny::sliderInput(
          "batch_Frule", "80% Rule Filter (%)",
          min = 0, max = 100, value = 80, step = 5
        ),
        htmltools::tags$p(class = "help-text",
          "Minimum percentage of non-missing values required per metabolite. 80% means a metabolite must be detected in at least 80% of samples. Set to 0% to disable."
        ),

        htmltools::tags$hr(),
        htmltools::tags$h6(class = "fw-semibold", "Output"),

        shiny::textInput(
          "batch_project_dir", "Project Directory (optional)",
          value = "", placeholder = "e.g., C:/Users/me/project"
        ),
        htmltools::tags$p(class = "help-text",
          "If set, corrected data and summary CSVs are saved to a ",
          htmltools::tags$code("batch_correction/"),
          " subfolder in this directory."
        ),

        shiny::actionButton(
          "batch_run", "Run Batch Correction",
          class = "btn-run btn-run-prominent w-100",
          icon = shiny::icon("play")
        )
      ),

      # Main panel
      htmltools::tags$div(
        class = "container-fluid py-3",

        htmltools::tags$div(
          class = "card mb-3",
          htmltools::tags$div(
            class = "card-header d-flex align-items-center gap-2",
            shiny::icon("terminal", class = "text-muted"),
            "Console Output"
          ),
          htmltools::tags$div(
            class = "card-body p-0",
            shiny::verbatimTextOutput("batch_console") |>
              htmltools::tagAppendAttributes(class = "console-output m-0")
          )
        ),

        bslib::navset_card_tab(
          id = "batch_results_tabs",
          title = "Batch Correction Results",

          bslib::nav_panel(
            title = htmltools::tagList(shiny::icon("chart-line"), "Signal Drift"),
            htmltools::tags$div(
              class = "p-3",
              shiny::uiOutput("batch_met_selector"),
              htmltools::tags$h6("Before Correction"),
              plotly::plotlyOutput("batch_plot_before", height = "400px"),
              htmltools::tags$hr(),
              htmltools::tags$h6("After Correction"),
              plotly::plotlyOutput("batch_plot_after", height = "400px"),
              htmltools::tags$p(class = "help-text",
                "Signal intensity over run order, coloured by sample type. ",
                "QC samples are shown in red with a LOESS trend line showing signal drift. ",
                "After correction the QC trend should be flat."
              ),
              empty_state("Run Batch Correction to see signal drift plots.", "batch_plot_before")
            )
          ),
          bslib::nav_panel(
            title = htmltools::tagList(shiny::icon("circle-nodes"), "PCA Overview"),
            htmltools::tags$div(
              class = "p-3",
              shiny::uiOutput("batch_pca_sample_toggle"),
              htmltools::tags$h6("Before Correction"),
              plotly::plotlyOutput("batch_pca_before", height = "400px"),
              htmltools::tags$hr(),
              htmltools::tags$h6("After Correction"),
              plotly::plotlyOutput("batch_pca_after", height = "400px"),
              htmltools::tags$p(class = "help-text",
                "PCA score plots coloured by sample type. QC samples shown in red. ",
                "Before correction sample types may cluster by batch; after correction they should overlap."
              ),
              empty_state("Run Batch Correction to see PCA plots.", "batch_pca_before")
            )
          ),
          bslib::nav_panel(
            title = htmltools::tagList(shiny::icon("percent"), "RSD Comparison"),
            htmltools::tags$div(
              class = "p-3",
              htmltools::tags$h6("Class Overview"),
              plotly::plotlyOutput("batch_rsd_class_plot", height = "400px"),
              htmltools::tags$p(class = "help-text",
                "Median QC %RSD by metabolite class (extracted from metabolite names). ",
                "Red = before, blue = after correction."
              ),
              htmltools::tags$hr(),
              htmltools::tags$h6("Per Metabolite"),
              shiny::uiOutput("batch_rsd_met_selector"),
              plotly::plotlyOutput("batch_rsd_plot", height = "350px"),
              htmltools::tags$p(class = "help-text",
                "QC %RSD before (red) vs after (blue) correction for the selected metabolite."
              ),
              htmltools::tags$hr(),
              DT::DTOutput("batch_rsd_table"),
              empty_state("Run Batch Correction to see RSD comparison.", "batch_rsd_plot")
            )
          ),
          bslib::nav_panel(
            title = htmltools::tagList(shiny::icon("table"), "Corrected Data"),
            htmltools::tags$div(
              class = "p-3",
              DT::DTOutput("batch_corrected_table"),
              empty_state("Run Batch Correction to see corrected data.", "batch_corrected_table")
            )
          )
        ),

        htmltools::tags$div(
          class = "mt-3",
          shiny::downloadButton("batch_download", "Download Corrected Data",
                                class = "btn btn-outline-secondary")
        )
      )
    )
  ),

  # ============================================================================
  # TAB 6: Results Explorer
  # ============================================================================
  bslib::nav_panel(
    title = "Results",
    icon  = shiny::icon("chart-bar"),
    htmltools::tags$div(
      class = "container-fluid py-4 results-page",

      # Hero header
      htmltools::tags$div(
        class = "results-hero mb-4",
        htmltools::tags$div(
          class = "results-hero-content",
          htmltools::tags$div(
            class = "d-flex align-items-center gap-3 mb-2",
            htmltools::tags$div(
              class = "results-hero-icon",
              shiny::icon("chart-bar")
            ),
            htmltools::tags$div(
              htmltools::tags$h4(class = "fw-bold mb-0", "Results Explorer"),
              htmltools::tags$p(
                class = "text-muted mb-0 mt-1",
                "Quality dashboard and interactive data exploration"
              )
            )
          )
        )
      ),

      # Sidebar + Main content layout
      htmltools::tags$div(
        class = "row g-4",

        # ── LEFT SIDEBAR: Controls ──
        htmltools::tags$div(
          class = "col-lg-3 col-xl-2",
          htmltools::tags$div(
            class = "results-sidebar",

            # Sidebar heading
            htmltools::tags$div(
              class = "results-sidebar-heading",
              shiny::icon("sliders", class = "me-2"),
              "Filters"
            ),

            # Data Source
            htmltools::tags$div(
              class = "results-sidebar-section",
              htmltools::tags$label(
                class = "form-label fw-semibold small text-uppercase",
                style = "letter-spacing: 0.05em;",
                shiny::icon("database", class = "me-1"),
                "Data Source"
              ),
              shiny::selectInput(
                "results_source", NULL,
                choices = c(
                  "QC-corrected data"    = "qc",
                  "Batch-corrected data" = "batch",
                  "Uploaded file"        = "upload"
                ),
                selected = "qc"
              )
            ),

            # Class
            htmltools::tags$div(
              class = "results-sidebar-section",
              htmltools::tags$label(
                class = "form-label fw-semibold small text-uppercase",
                style = "letter-spacing: 0.05em;",
                shiny::icon("layer-group", class = "me-1"),
                "Class"
              ),
              shiny::selectInput(
                "results_lipid_class", NULL,
                choices = c("All Classes" = "all"),
                selected = "all"
              )
            ),

            # QC Status
            htmltools::tags$div(
              class = "results-sidebar-section",
              htmltools::tags$label(
                class = "form-label fw-semibold small text-uppercase",
                style = "letter-spacing: 0.05em;",
                shiny::icon("filter", class = "me-1"),
                "QC Status"
              ),
              shiny::selectInput(
                "results_qc_filter", NULL,
                choices = c(
                  "All Metabolites" = "all",
                  "Passed Only"     = "pass",
                  "Failed Only"     = "fail",
                  "Warning Only"    = "warning"
                ),
                selected = "all"
              )
            ),

            # RSD Thresholds
            htmltools::tags$div(
              class = "results-sidebar-section",
              htmltools::tags$label(
                class = "form-label fw-semibold small text-uppercase",
                style = "letter-spacing: 0.05em;",
                shiny::icon("sliders", class = "me-1"),
                "RSD Thresholds"
              ),
              htmltools::tags$div(
                class = "results-threshold-sliders",
                shiny::sliderInput(
                  "results_rsd_warn", "Warning (%)",
                  min = 5, max = 50, value = 20, step = 5,
                  ticks = FALSE, width = "100%"
                ),
                shiny::sliderInput(
                  "results_rsd_fail", "Fail (%)",
                  min = 10, max = 80, value = 30, step = 5,
                  ticks = FALSE, width = "100%"
                )
              )
            ),

            # Divider
            htmltools::tags$hr(class = "my-2"),

            # Filter status badge
            shiny::uiOutput("results_filter_badge"),

            # Divider
            htmltools::tags$hr(class = "my-2"),

            # Export CSV
            htmltools::tags$div(
              class = "results-sidebar-section",
              shiny::downloadButton(
                "results_download_csv", "Export CSV",
                class = "btn btn-outline-primary w-100",
                icon = shiny::icon("download")
              )
            ),

            # Upload panel — shown only when "Uploaded file" source is selected
            shiny::conditionalPanel(
              condition = "input.results_source == 'upload'",
              htmltools::tags$hr(class = "my-2"),
              htmltools::tags$div(
                class = "results-sidebar-section",
                htmltools::tags$div(
                  class = "file-upload-area",
                  shiny::fileInput(
                    "results_upload_file", "Upload Data File",
                    accept = c(".xlsx", ".xls", ".csv", ".tsv"),
                    placeholder = "qcCheckR report (.xlsx) or corrected data (.csv)"
                  ),
                  htmltools::tags$p(
                    class = "help-text text-center small mt-n2",
                    "Drag & drop or click to browse"
                  )
                ),
                shiny::selectInput(
                  "results_upload_sheet", "Sheet (Excel)",
                  choices = c("Upload an Excel file first" = ""),
                  selected = ""
                ),
                shiny::actionButton(
                  "results_upload_load", "Load Data",
                  class = "btn btn-primary w-100 mt-2",
                  icon = shiny::icon("arrow-right-to-bracket")
                ),
                shiny::uiOutput("results_upload_status"),

                example_data_panel(
                  id = "results_upload_example",
                  title = "Expected data format",
                  description = paste0(
                    "Upload a qcCheckR Excel report and select a DATA sheet ",
                    "(e.g., DATA.all.concentration), or upload a batch-corrected CSV. ",
                    "Numeric columns are auto-detected as metabolites."
                  ),
                  columns = list(
                    "sample_name"         = "Sample identifier (enables per-sample labels)",
                    "sample_type_factor"  = "Sample type: qc, sample, blank (enables grouped plots); alternative: sample_type",
                    "sample_plate_id"     = "Batch/plate ID (optional); alternative: batch",
                    "sample_run_index"    = "Injection order (enables run-order plots); alternative: run_order",
                    "[metabolite cols]"   = "Numeric concentration or peak area values"
                  ),
                  rows = list(
                    c("PLASMA_LTR_01", "qc",     "PLATE_1", "1", "382663.0"),
                    c("Sample_001",    "sample", "PLATE_1", "2", "132454.2"),
                    c("Sample_002",    "sample", "PLATE_1", "3", "198712.8")
                  ),
                  notes = paste0(
                    "qcCheckR Excel sheets: DATA.all.concentration, ",
                    "DATA.preProcessed.concentration, DATA.all.concentration.S.T., ",
                    "DATA.preProcessed.conc.S.T., DATA.peakArea"
                  )
                )
              )
            )
          )
        ),

        # ── RIGHT MAIN CONTENT ──
        htmltools::tags$div(
          class = "col-lg-9 col-xl-10",

      # Summary value boxes — 6 boxes
      htmltools::tags$div(
        class = "row g-3 mb-4",
        # Box 1: Samples
        htmltools::tags$div(
          class = "col-6 col-lg-2",
          htmltools::tags$div(
            class = "value-box results-stat-box",
            htmltools::tags$div(
              class = "results-stat-icon results-stat-icon-primary",
              shiny::icon("vials")
            ),
            htmltools::tags$div(class = "value-box-title", "Samples"),
            shiny::uiOutput("results_n_samples")
          )
        ),
        # Box 2: Metabolites
        htmltools::tags$div(
          class = "col-6 col-lg-2",
          htmltools::tags$div(
            class = "value-box results-stat-box",
            htmltools::tags$div(
              class = "results-stat-icon results-stat-icon-info",
              shiny::icon("flask")
            ),
            htmltools::tags$div(class = "value-box-title", "Metabolites"),
            shiny::uiOutput("results_n_metabolites")
          )
        ),
        # Box 3: Median RSD
        htmltools::tags$div(
          class = "col-6 col-lg-2",
          htmltools::tags$div(
            class = "value-box results-stat-box",
            htmltools::tags$div(
              class = "results-stat-icon results-stat-icon-success",
              shiny::icon("percent")
            ),
            htmltools::tags$div(class = "value-box-title", "Median QC RSD"),
            shiny::uiOutput("results_median_rsd")
          )
        ),
        # Box 4: Missing %
        htmltools::tags$div(
          class = "col-6 col-lg-2",
          htmltools::tags$div(
            class = "value-box results-stat-box",
            htmltools::tags$div(
              class = "results-stat-icon results-stat-icon-warning",
              shiny::icon("triangle-exclamation")
            ),
            htmltools::tags$div(class = "value-box-title", "Missing %"),
            shiny::uiOutput("results_pct_missing")
          )
        ),
        # Box 5: Batch Correction
        htmltools::tags$div(
          class = "col-6 col-lg-2",
          htmltools::tags$div(
            class = "value-box results-stat-box",
            htmltools::tags$div(
              class = "results-stat-icon results-stat-icon-success",
              shiny::icon("arrows-rotate")
            ),
            htmltools::tags$div(class = "value-box-title", "Batch Correction"),
            shiny::uiOutput("results_batch_status")
          )
        ),
        # Box 6: Lipid Classes
        htmltools::tags$div(
          class = "col-6 col-lg-2",
          htmltools::tags$div(
            class = "value-box results-stat-box",
            htmltools::tags$div(
              class = "results-stat-icon results-stat-icon-info",
              shiny::icon("layer-group")
            ),
            htmltools::tags$div(class = "value-box-title", "Classes"),
            shiny::uiOutput("results_n_classes")
          )
        )
      ),

      # Tabbed plot area — 5 tabs
      bslib::navset_card_pill(
        id = "results_plot_tabs",

        # Tab 1: Quality Dashboard
        bslib::nav_panel(
          title = htmltools::tagList(shiny::icon("gauge-high"), "Quality Dashboard"),
          htmltools::tags$div(
            class = "p-3",
            htmltools::tags$div(
              class = "row mb-4",
              htmltools::tags$div(
                class = "col-md-7",
                plotly::plotlyOutput("results_rsd_histogram", height = "350px"),
                empty_state("No data yet. Run the QC Check tab first, or select 'Uploaded file' to load your own data.", "results_rsd_histogram")
              ),
              htmltools::tags$div(
                class = "col-md-5",
                plotly::plotlyOutput("results_passfail_donut", height = "350px"),
                empty_state("Pass/fail breakdown will appear once QC data is available.", "results_passfail_donut")
              )
            ),
            htmltools::tags$div(
              class = "row",
              htmltools::tags$div(
                class = "col-12",
                plotly::plotlyOutput("results_class_summary", height = "400px"),
                empty_state("Class breakdown will appear once lipid class information is detected.", "results_class_summary")
              )
            )
          )
        ),

        # Tab 2: RSD Explorer
        bslib::nav_panel(
          title = htmltools::tagList(shiny::icon("chart-simple"), "RSD Explorer"),
          htmltools::tags$div(
            class = "p-3",
            shiny::uiOutput("results_rsd_scatter_ui"),
            htmltools::tags$div(
              class = "row mb-4",
              htmltools::tags$div(
                class = "col-12",
                plotly::plotlyOutput("results_conc_vs_rsd", height = "400px")
              )
            ),
            plotly::plotlyOutput("results_rsd_bar", height = "650px"),
            empty_state("RSD bar chart will appear once QC data is loaded. Try the QC Check tab or upload a file.", "results_rsd_bar")
          )
        ),

        # Tab 3: Metabolite Deep Dive
        bslib::nav_panel(
          title = htmltools::tagList(shiny::icon("microscope"), "Metabolite Deep Dive"),
          htmltools::tags$div(
            class = "p-3",
            htmltools::tags$div(
              class = "row mb-3",
              htmltools::tags$div(
                class = "col-md-6",
                shiny::selectizeInput(
                  "results_deep_metabolite", "Select Metabolite",
                  choices = c("Run pipeline first" = ""),
                  selected = "",
                  options = list(
                    placeholder = "Type to search...",
                    maxOptions = 500
                  )
                )
              )
            ),
            shiny::uiOutput("results_metabolite_info"),
            htmltools::tags$div(
              class = "row mb-3",
              htmltools::tags$div(
                class = "col-md-6",
                plotly::plotlyOutput("results_boxplot", height = "400px")
              ),
              htmltools::tags$div(
                class = "col-md-6",
                plotly::plotlyOutput("results_runorder", height = "400px")
              )
            ),
            shiny::uiOutput("results_before_after_ui"),
            empty_state("Load data and select a metabolite from the dropdown above to see boxplots and run-order trends.", "results_boxplot")
          )
        ),

        # Tab 4: Heatmap
        bslib::nav_panel(
          title = htmltools::tagList(shiny::icon("grip"), "Heatmap"),
          htmltools::tags$div(
            class = "p-3",
            htmltools::tags$div(
              class = "row mb-3",
              htmltools::tags$div(
                class = "col-md-4",
                shiny::radioButtons(
                  "results_heatmap_scope", "Scope",
                  choices = c("Filtered Metabolites" = "filtered",
                              "Top 30 by Variance"   = "top30"),
                  selected = "filtered", inline = TRUE
                )
              ),
              htmltools::tags$div(
                class = "col-md-4",
                shiny::radioButtons(
                  "results_heatmap_samples", "Samples",
                  choices = c("All Samples"       = "all",
                              "QC Only"            = "qc",
                              "Study Samples Only" = "samples"),
                  selected = "all", inline = TRUE
                )
              )
            ),
            plotly::plotlyOutput("results_heatmap", height = "650px"),
            empty_state("Heatmap will appear once data is loaded. Run QC Check, Batch Correction, or upload a file.", "results_heatmap")
          )
        ),

        # Tab 5: Data Table
        bslib::nav_panel(
          title = htmltools::tagList(shiny::icon("table"), "Data Table"),
          htmltools::tags$div(
            class = "p-3",
            htmltools::tags$div(
              class = "d-flex align-items-center justify-content-between mb-3",
              htmltools::tags$div(
                shiny::radioButtons(
                  "results_table_view", NULL,
                  choices = c(
                    "Concentration Data" = "concentration",
                    "RSD Summary"        = "rsd_summary",
                    "Failed Metabolites" = "failed_mets",
                    "Failed Samples"     = "failed_samples",
                    "Missing Values"     = "missing_values",
                    "Sample Quality"     = "sample_quality"
                  ),
                  selected = "concentration", inline = TRUE
                )
              ),
              shiny::downloadButton(
                "results_download_table", "Export Table",
                class = "btn btn-sm btn-outline-primary",
                icon = shiny::icon("download")
              )
            ),
            DT::DTOutput("results_data_table"),
            empty_state("Data tables will populate once results are available. Switch views using the buttons above.", "results_data_table")
          )
        )
      )

        ) # end col-lg-9 (main content)
      ) # end row g-4 (sidebar + main)
    )
  ),

  # ============================================================================
  # TAB 7: Utilities
  # ============================================================================
  bslib::nav_panel(
    title = "Utilities",
    icon  = shiny::icon("wrench"),
    htmltools::tags$div(
      class = "container-fluid py-4",

      htmltools::tags$div(
        class = "row",

        # Transition Checker
        htmltools::tags$div(
          class = "col-lg-6 mb-4",
          htmltools::tags$div(
            class = "card h-100 utility-card utility-card-teal",
            htmltools::tags$div(
              class = "card-header fw-semibold d-flex align-items-center gap-2",
              shiny::icon("exchange-alt", class = "text-info"),
              "Transition Checker (transition_checkR)"
            ),
            htmltools::tags$div(
              class = "card-body",
              htmltools::tags$p(
                class = "text-muted mb-3",
                "Validate that all Q1/Q3 MRM transitions are unique."
              ),
              htmltools::tags$div(
                class = "file-upload-area",
                shiny::fileInput(
                  "util_transition_file", "Upload MRM Template",
                  accept = c(".tsv", ".csv", ".txt")
                ),
                htmltools::tags$p(class = "help-text text-center mt-n2",
                                   "Drag & drop file here or click to browse")
              ),
              shiny::actionButton(
                "util_transition_run", "Check Transitions",
                class = "btn-run",
                icon = shiny::icon("check-circle")
              ),
              htmltools::tags$hr(),
              shiny::uiOutput("util_transition_result"),
              DT::DTOutput("util_transition_table")
            )
          )
        ),

        # Template Comparator
        htmltools::tags$div(
          class = "col-lg-6 mb-4",
          htmltools::tags$div(
            class = "card h-100 utility-card utility-card-blue",
            htmltools::tags$div(
              class = "card-header fw-semibold d-flex align-items-center gap-2",
              shiny::icon("code-compare", class = "text-primary"),
              "Template Comparator (compare_mrm_template_with_guide)"
            ),
            htmltools::tags$div(
              class = "card-body",
              htmltools::tags$p(
                class = "text-muted mb-3",
                "Check that all internal standards in the MRM template ",
                "have matching entries in the concentration guide."
              ),
              htmltools::tags$div(
                class = "file-upload-area",
                shiny::fileInput(
                  "util_compare_mrm", "Upload MRM Template",
                  accept = c(".tsv", ".csv", ".txt")
                ),
                htmltools::tags$p(class = "help-text text-center mt-n2",
                                   "Drag & drop file here or click to browse")
              ),
              htmltools::tags$div(
                class = "file-upload-area",
                shiny::fileInput(
                  "util_compare_conc", "Upload Concentration Guide",
                  accept = c(".tsv", ".csv", ".txt")
                ),
                htmltools::tags$p(class = "help-text text-center mt-n2",
                                   "Drag & drop file here or click to browse")
              ),
              shiny::actionButton(
                "util_compare_run", "Compare Templates",
                class = "btn-run",
                icon = shiny::icon("code-compare")
              ),
              htmltools::tags$hr(),
              shiny::uiOutput("util_compare_result"),
              DT::DTOutput("util_compare_table")
            )
          )
        )
      ),

      # Dependency Checker
      htmltools::tags$div(
        class = "row",
        htmltools::tags$div(
          class = "col-12 mb-4",
          htmltools::tags$div(
            class = "card utility-card utility-card-green",
            htmltools::tags$div(
              class = "card-header fw-semibold d-flex align-items-center gap-2",
              shiny::icon("puzzle-piece", class = "text-success"),
              "Dependency Checker"
            ),
            htmltools::tags$div(
              class = "card-body",
              htmltools::tags$p(
                class = "text-muted mb-3",
                "Verify that all required R packages and system ",
                "dependencies are installed."
              ),
              shiny::actionButton(
                "util_depcheck_run", "Check Dependencies",
                class = "btn-run",
                icon = shiny::icon("list-check")
              ),
              htmltools::tags$hr(),
              DT::DTOutput("util_depcheck_table")
            )
          )
        )
      )
    )
  ),

  # ============================================================================
  # TAB 8: User Guide
  # ============================================================================
  bslib::nav_panel(
    title = "Guide",
    icon  = shiny::icon("book-open"),
    htmltools::tags$div(
      class = "container-fluid py-4",
      htmltools::tags$div(
        class = "row justify-content-center",
        htmltools::tags$div(
          class = "col-lg-10",

          htmltools::tags$div(
            class = "card mb-4",
            htmltools::tags$div(
              class = "card-header fw-semibold d-flex align-items-center gap-2",
              shiny::icon("route", class = "text-primary"),
              "Quick Start Workflow"
            ),
            htmltools::tags$div(
              class = "card-body",
              htmltools::tags$p("MStargetR processes targeted LC-MS (MRM) data in 3 steps:"),
              htmltools::tags$ol(
                htmltools::tags$li(
                  htmltools::tags$strong("File Conversion"), " — Convert vendor files (.wiff, .raw, .d) to open mzML format using Docker + msConvert."
                ),
                htmltools::tags$li(
                  htmltools::tags$strong("Peak Integration"), " — Integrate chromatographic peaks using Skyline via Docker. Requires MRM template file(s)."
                ),
                htmltools::tags$li(
                  htmltools::tags$strong("Quality Control"), " — QC assessment, batch correction, filtering, and concentration calculation. Requires SIL guide + concentration guide files."
                )
              ),
              htmltools::tags$p(
                class = "text-muted",
                "Each step outputs to a structured project directory. ",
                "You can also use Batch Correction as a standalone tool."
              )
            )
          ),

          htmltools::tags$div(
            class = "card mb-4",
            htmltools::tags$div(
              class = "card-header fw-semibold d-flex align-items-center gap-2",
              shiny::icon("folder-tree", class = "text-info"),
              "Project Directory Structure"
            ),
            htmltools::tags$div(
              class = "card-body",
              htmltools::tags$pre(
                class = "console-output",
                style = "max-height:250px;font-size:0.82rem;",
                paste0(
                  "project_directory/\n",
                  "  raw_data/            # Place vendor files here\n",
                  "  PLATE_ID/\n",
                  "    data/\n",
                  "      mzml/            # Converted mzML files\n",
                  "      raw_data/        # Copied vendor files\n",
                  "      rda/             # R data objects\n",
                  "    reports/           # Skyline reports\n",
                  "  all/\n",
                  "    data/rda/          # Combined QC results\n",
                  "    xlsx_report/       # Excel summary reports\n",
                  "  archive/             # Archived raw data\n",
                  "  MStargetR_logs/      # Pipeline log files"
                )
              )
            )
          ),

          htmltools::tags$div(
            class = "card mb-4",
            htmltools::tags$div(
              class = "card-header fw-semibold d-flex align-items-center gap-2",
              shiny::icon("tags", class = "text-success"),
              "Understanding Sample Tags & QC Labels"
            ),
            htmltools::tags$div(
              class = "card-body",
              htmltools::tags$p(
                "MStargetR identifies sample types by searching for ",
                htmltools::tags$strong("tags"), " within your file names. ",
                "For example, a file named ", htmltools::tags$code("batch1_PQC_001.mzML"),
                " contains the tag ", htmltools::tags$code("PQC"), "."
              ),
              htmltools::tags$h6(class = "fw-semibold mt-3", "Sample Tags"),
              htmltools::tags$p(
                "A comma-separated list of ALL tags that appear in your file names. ",
                "Common tags: ", htmltools::tags$code("sample, blank, PQC, LTR, QC, VLTR"),
                ". Matching is case-insensitive and uses underscore boundaries."
              ),
              htmltools::tags$h6(class = "fw-semibold mt-3", "QC Sample Label"),
              htmltools::tags$p(
                "The specific tag for your Quality Control samples. ",
                "This MUST also appear in the Sample Tags list. ",
                "If you have multiple QC types (e.g., PQC and LTR), choose the primary one here."
              ),
              htmltools::tags$div(
                class = "alert alert-info",
                shiny::icon("circle-info"),
                " Example: If your files are named ",
                htmltools::tags$code("plate1_PQC_001, plate1_sample_002, plate1_blank_003"),
                ", set Sample Tags to ", htmltools::tags$code("sample,blank,pqc"),
                " and QC Label to ", htmltools::tags$code("PQC"), "."
              )
            )
          ),

          htmltools::tags$div(
            class = "card mb-4",
            htmltools::tags$div(
              class = "card-header fw-semibold d-flex align-items-center gap-2",
              shiny::icon("docker", lib = "font-awesome", class = "text-primary"),
              "Docker Requirements"
            ),
            htmltools::tags$div(
              class = "card-body",
              htmltools::tags$p(
                "File Conversion and Peak Integration require ",
                htmltools::tags$strong("Docker Desktop"), " to be installed and running."
              ),
              htmltools::tags$ul(
                htmltools::tags$li("Download from docker.com/products/docker-desktop"),
                htmltools::tags$li("Minimum 8 GB RAM allocated to Docker"),
                htmltools::tags$li("The ProteoWizard image is downloaded automatically on first use (~2 GB)"),
                htmltools::tags$li("Apple Silicon (M1/M2/M3) Macs may experience slower performance via Rosetta 2")
              )
            )
          ),

          htmltools::tags$div(
            class = "card mb-4",
            htmltools::tags$div(
              class = "card-header fw-semibold d-flex align-items-center gap-2",
              shiny::icon("keyboard", class = "text-muted"),
              "Keyboard Shortcuts"
            ),
            htmltools::tags$div(
              class = "card-body",
              htmltools::tags$table(
                class = "table table-sm table-hover mb-0",
                htmltools::tags$thead(
                  htmltools::tags$tr(
                    htmltools::tags$th("Shortcut"),
                    htmltools::tags$th("Action")
                  )
                ),
                htmltools::tags$tbody(
                  htmltools::tags$tr(htmltools::tags$td(htmltools::tags$kbd("Ctrl+Shift+D")), htmltools::tags$td("Toggle dark/light theme")),
                  htmltools::tags$tr(htmltools::tags$td(htmltools::tags$kbd("Ctrl+Shift+1-9")), htmltools::tags$td("Navigate to tab by number"))
                )
              )
            )
          ),

          htmltools::tags$div(
            class = "card mb-5 pb-4",
            htmltools::tags$div(
              class = "card-header fw-semibold d-flex align-items-center gap-2",
              shiny::icon("circle-question", class = "text-warning"),
              "Troubleshooting"
            ),
            htmltools::tags$div(
              class = "card-body",
              htmltools::tags$h6(class = "fw-semibold", "\"No QC type found\""),
              htmltools::tags$p("Ensure your QC label (e.g., PQC) is included in the Sample Tags list AND appears in your sample file names with underscore boundaries."),
              htmltools::tags$h6(class = "fw-semibold mt-3", "Docker not found"),
              htmltools::tags$p("Start Docker Desktop, wait for it to fully load, then click the Docker status refresh or restart the app."),
              htmltools::tags$h6(class = "fw-semibold mt-3", "Conversion stuck / no output"),
              htmltools::tags$p("Check the console output for error messages. Ensure your raw_data folder contains supported vendor files (.wiff, .raw, .d).")
            )
          )

        )
      )
    )
  ),

  # ============================================================================
  # TAB 9: Settings
  # ============================================================================
  bslib::nav_panel(
    title = "Settings",
    icon  = shiny::icon("gear"),
    htmltools::tags$div(
      class = "container-fluid py-4",
      htmltools::tags$div(
        class = "row justify-content-center",
        htmltools::tags$div(
          class = "col-lg-8",

          htmltools::tags$div(
            class = "card mb-4 settings-card",
            htmltools::tags$div(
              class = "card-header fw-semibold d-flex align-items-center gap-2",
              shiny::icon("user", class = "text-primary"),
              "User Defaults"
            ),
            htmltools::tags$div(
              class = "card-body",
              htmltools::tags$p(
                class = "help-text mb-3",
                "Set your default user name and sample identification tags. ",
                "These will pre-populate fields across all pipeline tabs."
              ),
              shiny::textInput(
                "settings_user_name", "Default User Name",
                placeholder = "Your name or identifier"
              ),
              shiny::textInput(
                "settings_qc_label", "Default QC Label",
                value = "LTR",
                placeholder = "e.g., LTR, QC"
              ),
              htmltools::tags$p(
                class = "help-text",
                "Label used to identify quality control samples in file names."
              ),
              shiny::textInput(
                "settings_sample_tags", "Default Sample Tags",
                value = "sample,control,blank,qc",
                placeholder = "Comma-separated tags"
              ),
              htmltools::tags$p(
                class = "help-text",
                "Comma-separated list of tags used to classify sample types."
              )
            )
          ),

          htmltools::tags$div(
            class = "card mb-4 settings-card",
            htmltools::tags$div(
              class = "card-header fw-semibold d-flex align-items-center gap-2",
              shiny::icon("palette", class = "text-info"),
              "Appearance"
            ),
            htmltools::tags$div(
              class = "card-body",
              htmltools::tags$p(
                class = "help-text mb-3",
                "Customize the visual appearance of the application."
              ),
              shiny::radioButtons(
                "settings_theme", "Theme",
                choices = c("Light" = "light", "Dark" = "dark"),
                selected = "light",
                inline = TRUE
              ),
              htmltools::tags$p(
                class = "help-text",
                "You can also toggle the theme with the moon/sun button in the ",
                "navigation bar, or with Ctrl+Shift+D."
              )
            )
          ),

          htmltools::tags$div(
            class = "card mb-4 settings-card",
            htmltools::tags$div(
              class = "card-header fw-semibold d-flex align-items-center gap-2",
              shiny::icon("server", class = "text-warning"),
              "Docker Configuration"
            ),
            htmltools::tags$div(
              class = "card-body",
              htmltools::tags$p(
                class = "help-text mb-3",
                "Configure the Docker connection used for file conversion and peak integration."
              ),
              shiny::textInput(
                "settings_docker_path", "Docker Executable Path",
                value = "docker",
                placeholder = "docker (default) or full path"
              ),
              htmltools::tags$p(
                class = "help-text",
                "Usually 'docker' is sufficient. Provide a full path only if ",
                "Docker is not on your system PATH."
              ),
              shiny::uiOutput("settings_docker_test")
            )
          ),

          htmltools::tags$div(
            class = "d-flex gap-3 mb-5 pb-4",
            shiny::actionButton(
              "settings_save", "Save Settings",
              class = "btn-primary",
              icon = shiny::icon("floppy-disk")
            ),
            shiny::actionButton(
              "settings_reset", "Reset to Defaults",
              class = "btn btn-outline-secondary",
              icon = shiny::icon("rotate-left")
            )
          )
        )
      )
    )
  )
)
