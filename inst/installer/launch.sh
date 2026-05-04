#!/usr/bin/env bash
# MStargetR Launcher (macOS / Linux)
# =============================================================================
# This script finds Rscript and launches the MStargetR Shiny GUI.
# Usage: bash launch.sh   (or double-click on macOS if saved as .command)

set -euo pipefail

echo ""
echo "============================================"
echo "  MStargetR - Targeted LC-MS Preprocessing"
echo "============================================"
echo ""

# ---------------------------------------------------------------------------
# Step 1: Find Rscript
# ---------------------------------------------------------------------------

RSCRIPT=""

# Check PATH first
if command -v Rscript &>/dev/null; then
    RSCRIPT="$(command -v Rscript)"
fi

# macOS: check common Homebrew and CRAN locations
if [[ -z "$RSCRIPT" && "$(uname)" == "Darwin" ]]; then
    for dir in /usr/local/bin /opt/homebrew/bin /Library/Frameworks/R.framework/Versions/*/Resources/bin; do
        if [[ -x "$dir/Rscript" ]]; then
            RSCRIPT="$dir/Rscript"
            break
        fi
    done
fi

# Linux: check common locations
if [[ -z "$RSCRIPT" && "$(uname)" == "Linux" ]]; then
    for dir in /usr/bin /usr/local/bin /opt/R/*/bin; do
        if [[ -x "$dir/Rscript" ]]; then
            RSCRIPT="$dir/Rscript"
            break
        fi
    done
fi

if [[ -z "$RSCRIPT" ]]; then
    echo "ERROR: Rscript not found."
    echo ""
    if [[ "$(uname)" == "Darwin" ]]; then
        echo "Install R on macOS:"
        echo "  Option 1: brew install r"
        echo "  Option 2: Download from https://cran.r-project.org/bin/macosx/"
    else
        echo "Install R on Linux:"
        echo "  Ubuntu/Debian: sudo apt install r-base"
        echo "  Fedora/RHEL:   sudo dnf install R"
        echo "  Or download from https://cran.r-project.org/"
    fi
    echo ""
    exit 1
fi

echo "Found R: $RSCRIPT"
"$RSCRIPT" --version 2>&1 | head -1
echo ""

# ---------------------------------------------------------------------------
# Step 2: Determine project directory
# ---------------------------------------------------------------------------

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Go up from inst/installer to project root
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "Project: $PROJECT_DIR"
echo ""

# ---------------------------------------------------------------------------
# Step 3: Check Docker (optional, non-blocking)
# ---------------------------------------------------------------------------

if command -v docker &>/dev/null; then
    if docker info &>/dev/null 2>&1; then
        echo "Docker: available and running"
    else
        echo "Docker: installed but daemon not running"
        echo "  (Docker is only needed for File Conversion and Peak Integration)"
    fi
else
    echo "Docker: not installed"
    echo "  (Docker is only needed for File Conversion and Peak Integration)"
    if [[ "$(uname)" == "Darwin" ]]; then
        echo "  Install: https://www.docker.com/products/docker-desktop/"
    else
        echo "  Install: https://docs.docker.com/engine/install/"
    fi
fi

echo ""
echo "Starting MStargetR GUI..."
echo "Your browser will open automatically."
echo "Press Ctrl+C to stop the application."
echo ""

# ---------------------------------------------------------------------------
# Step 4: Launch
# ---------------------------------------------------------------------------

"$RSCRIPT" --vanilla -e "
args <- commandArgs(trailingOnly = TRUE)
projdir <- normalizePath(args[1], winslash = '/')
descfile <- file.path(projdir, 'DESCRIPTION')

if (file.exists(descfile) && requireNamespace('devtools', quietly = TRUE)) {
  cat('Development mode: loading from source...\n')
  devtools::load_all(projdir, quiet = TRUE)
  appDir <- system.file('shiny', 'MStargetR_app', package = 'MStargetR')
  if (!nzchar(appDir)) appDir <- file.path(projdir, 'inst', 'shiny', 'MStargetR_app')
  if (dir.exists(appDir)) {
    shiny::runApp(appDir, launch.browser = TRUE, host = '127.0.0.1')
  } else {
    stop('Shiny app directory not found at: ', appDir)
  }
} else if (requireNamespace('MStargetR', quietly = TRUE)) {
  cat('Launching installed MStargetR package...\n')
  MStargetR::launchMStargetR(launch.browser = TRUE, host = '127.0.0.1')
} else {
  stop('MStargetR not found. Install with: remotes::install_github(\"MStargetR/MStargetR\")')
}
" --args "$PROJECT_DIR"

echo ""
echo "MStargetR has stopped."
