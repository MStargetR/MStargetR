#!/usr/bin/env bash
# MStargetR One-Step Installer (macOS / Linux)
# =============================================================================
# This script checks for R, installs dependencies, and launches MStargetR.
# Designed for users who may not have R or Docker installed.
#
# Usage:
#   bash install.sh              # Install and launch
#   bash install.sh --no-launch  # Install only, don't launch the GUI

set -euo pipefail

NO_LAUNCH=false
if [[ "${1:-}" == "--no-launch" ]]; then
    NO_LAUNCH=true
fi

OS="$(uname)"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Resolve project root explicitly so symlinks / odd mounts fail loudly
# rather than silently falling back to the installer directory (which
# breaks the bundled-source install path below).
if ! PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"; then
    echo "ERROR: Cannot resolve project root relative to $SCRIPT_DIR" >&2
    exit 1
fi

echo ""
echo "============================================"
echo "  MStargetR Installer"
echo "============================================"
echo ""
echo "Platform: $OS"
echo "Project:  $PROJECT_DIR"
echo ""

# ---------------------------------------------------------------------------
# Step 1: Check / Install R
# ---------------------------------------------------------------------------

echo "[1/3] Checking for R..."

RSCRIPT=""
if command -v Rscript &>/dev/null; then
    RSCRIPT="$(command -v Rscript)"
fi

# macOS: check CRAN framework location
if [[ -z "$RSCRIPT" && "$OS" == "Darwin" ]]; then
    for dir in /Library/Frameworks/R.framework/Versions/*/Resources/bin /opt/homebrew/bin /usr/local/bin; do
        if [[ -x "$dir/Rscript" ]]; then
            RSCRIPT="$dir/Rscript"
            break
        fi
    done
fi

# Linux: check common locations
if [[ -z "$RSCRIPT" && "$OS" == "Linux" ]]; then
    for dir in /usr/bin /usr/local/bin /opt/R/*/bin; do
        if [[ -x "$dir/Rscript" ]]; then
            RSCRIPT="$dir/Rscript"
            break
        fi
    done
fi

if [[ -z "$RSCRIPT" ]]; then
    echo ""
    echo "  R is not installed."
    echo ""

    if [[ "$OS" == "Darwin" ]]; then
        # macOS: try Homebrew, then prompt for CRAN
        if command -v brew &>/dev/null; then
            echo "  Installing R via Homebrew..."
            brew install r
            RSCRIPT="$(command -v Rscript || echo "")"
        fi

        if [[ -z "$RSCRIPT" ]]; then
            echo "  Please install R from: https://cran.r-project.org/bin/macosx/"
            echo "  Or install Homebrew (https://brew.sh) and run: brew install r"
            echo ""
            echo "  After installing R, run this script again."
            exit 1
        fi

    elif [[ "$OS" == "Linux" ]]; then
        # Linux: detect package manager and install
        if command -v apt-get &>/dev/null; then
            echo "  Installing R via apt..."
            echo "  (You may be prompted for your password)"
            sudo apt-get update -qq
            sudo apt-get install -y r-base r-base-dev libcurl4-openssl-dev \
                libssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev \
                libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
            RSCRIPT="$(command -v Rscript || echo "")"
        elif command -v dnf &>/dev/null; then
            echo "  Installing R via dnf..."
            sudo dnf install -y R-core R-core-devel \
                libcurl-devel openssl-devel libxml2-devel
            RSCRIPT="$(command -v Rscript || echo "")"
        elif command -v yum &>/dev/null; then
            echo "  Installing R via yum..."
            sudo yum install -y R-core R-core-devel \
                libcurl-devel openssl-devel libxml2-devel
            RSCRIPT="$(command -v Rscript || echo "")"
        fi

        if [[ -z "$RSCRIPT" ]]; then
            echo "  Could not auto-install R."
            echo "  Please install R from: https://cran.r-project.org/"
            echo ""
            echo "  Ubuntu/Debian:  sudo apt install r-base r-base-dev"
            echo "  Fedora/RHEL:    sudo dnf install R-core R-core-devel"
            echo ""
            echo "  After installing R, run this script again."
            exit 1
        fi
    else
        echo "  Unsupported OS: $OS"
        echo "  Please install R from: https://cran.r-project.org/"
        exit 1
    fi
fi

echo "  R found: $RSCRIPT"
"$RSCRIPT" --version 2>&1 | head -1
echo ""

# ---------------------------------------------------------------------------
# Step 2: Install R packages
# ---------------------------------------------------------------------------

echo "[2/3] Installing R packages..."
echo "  (This may take several minutes on first run)"
echo ""

INSTALL_SCRIPT="$SCRIPT_DIR/install_deps.R"
if [[ -f "$INSTALL_SCRIPT" ]]; then
    "$RSCRIPT" "$INSTALL_SCRIPT"
else
    echo "  install_deps.R not found at $INSTALL_SCRIPT"
    echo "  Attempting minimal install..."
    "$RSCRIPT" -e "
if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes', repos = 'https://cloud.r-project.org')
if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager', repos = 'https://cloud.r-project.org')
remotes::install_github('MStargetR/MStargetR', upgrade = 'never')
cat('MStargetR installed successfully.\n')
"
fi

echo ""

# ---------------------------------------------------------------------------
# Step 3: Check Docker (optional)
# ---------------------------------------------------------------------------

echo "[3/3] Checking Docker (optional)..."

if command -v docker &>/dev/null; then
    if docker info &>/dev/null 2>&1; then
        echo "  Docker: available and running"
    else
        echo "  Docker: installed but daemon is not running"
    fi
else
    echo "  Docker: not installed (optional)"
    echo "  Docker is only needed for File Conversion and Peak Integration."
    if [[ "$OS" == "Darwin" ]]; then
        echo "  Install from: https://www.docker.com/products/docker-desktop/"
    else
        echo "  Install from: https://docs.docker.com/engine/install/"
    fi
fi

echo ""
echo "============================================"
echo "  Installation complete!"
echo "============================================"
echo ""

# ---------------------------------------------------------------------------
# Step 4: Launch (unless --no-launch)
# ---------------------------------------------------------------------------

if [[ "$NO_LAUNCH" == true ]]; then
    echo "To launch MStargetR, run:"
    echo "  bash $SCRIPT_DIR/launch.sh"
    exit 0
fi

echo "Launching MStargetR..."
echo "Your browser will open automatically."
echo "Press Ctrl+C to stop."
echo ""

bash "$SCRIPT_DIR/launch.sh"
