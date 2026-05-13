# MStargetR Prerequisite Checker and Installer
# =============================================================================
# Usage: powershell -ExecutionPolicy Bypass -File check_prerequisites.ps1
#        [-InstallR true|false] [-InstallDocker true|false]

param(
    [string]$InstallR = "false",
    [string]$InstallDocker = "false"
)

$ErrorActionPreference = "Continue"
# Write log to user-writable location (PSScriptRoot may be in Program Files)
$LogDir = Join-Path $env:LOCALAPPDATA "MStargetR"
if (-not (Test-Path $LogDir)) { New-Item -ItemType Directory -Path $LogDir -Force | Out-Null }
$LogFile = Join-Path $LogDir "install_log.txt"

function Write-Log {
    param([string]$Message)
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $entry = "$timestamp - $Message"
    Add-Content -Path $LogFile -Value $entry
    Write-Host $entry
}

# ---------------------------------------------------------------------------
# R Installation
# ---------------------------------------------------------------------------

function Find-R {
    # Check PATH
    $rscript = Get-Command "Rscript" -ErrorAction SilentlyContinue
    if ($rscript) { return $rscript.Source }

    # Check Windows Registry for R installation path
    $regPaths = @(
        "HKLM:\SOFTWARE\R-core\R",
        "HKLM:\SOFTWARE\WOW6432Node\R-core\R",
        "HKCU:\SOFTWARE\R-core\R"
    )
    foreach ($regPath in $regPaths) {
        try {
            $installPath = (Get-ItemProperty -Path $regPath -ErrorAction SilentlyContinue).InstallPath
            if ($installPath -and (Test-Path $installPath)) {
                $rscript_path = Join-Path $installPath "bin\Rscript.exe"
                if (Test-Path $rscript_path) {
                    Write-Log "Found R via registry: $rscript_path"
                    return $rscript_path
                }
            }
        } catch { }
    }

    # Check common install locations
    $locations = @(
        "C:\Program Files\R",
        "$env:LOCALAPPDATA\Programs\R",
        "$env:ProgramFiles\R"
    )

    foreach ($loc in $locations) {
        if (Test-Path $loc) {
            $versions = Get-ChildItem $loc -Directory | Sort-Object Name -Descending
            foreach ($ver in $versions) {
                $rscript_path = Join-Path $ver.FullName "bin\Rscript.exe"
                if (Test-Path $rscript_path) {
                    return $rscript_path
                }
            }
        }
    }

    return $null
}

function Install-R {
    Write-Log "Detecting latest R version from CRAN..."

    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

    # Dynamically detect the latest R version from CRAN
    $r_version = $null
    try {
        $page = Invoke-WebRequest -Uri "https://cran.r-project.org/bin/windows/base/" -UseBasicParsing
        if ($page.Content -match 'R-(\d+\.\d+\.\d+)-win\.exe') {
            $r_version = $Matches[1]
        }
    } catch {
        Write-Log "WARNING: Could not detect latest R version from CRAN."
    }

    # Fallback to a known-good version (update periodically: https://cran.r-project.org/)
    if (-not $r_version) {
        $r_version = "4.5.0"
        Write-Log "Using fallback R version: $r_version"
    } else {
        Write-Log "Latest R version detected: $r_version"
    }

    $r_url = "https://cran.r-project.org/bin/windows/base/R-$r_version-win.exe"
    $r_installer = Join-Path $env:TEMP "R-$r_version-win.exe"

    try {
        Write-Log "Downloading R $r_version..."
        Invoke-WebRequest -Uri $r_url -OutFile $r_installer -UseBasicParsing

        Write-Log "Installing R $r_version (silent)..."
        # Try user-level install first (no admin required)
        $r_dir = "$env:LOCALAPPDATA\Programs\R\R-$r_version"
        $proc = Start-Process -FilePath $r_installer -ArgumentList "/VERYSILENT /NORESTART /DIR=`"$r_dir`"" -Wait -PassThru

        if ($proc.ExitCode -eq 0) {
            Write-Log "R $r_version installed successfully to $r_dir"

            # Add to PATH for this session
            $rbin = Join-Path $r_dir "bin"
            if (Test-Path $rbin) {
                $env:PATH = "$rbin;$env:PATH"
            }
        } else {
            Write-Log "R installation returned exit code: $($proc.ExitCode)"
        }
    } catch {
        Write-Log "ERROR: Failed to download/install R: $_"
        Write-Log "Please install R manually from https://cran.r-project.org/"
    } finally {
        if (Test-Path $r_installer) {
            Remove-Item $r_installer -Force -ErrorAction SilentlyContinue
        }
    }
}

# ---------------------------------------------------------------------------
# Docker Check
# ---------------------------------------------------------------------------

function Find-Docker {
    $docker = Get-Command "docker" -ErrorAction SilentlyContinue
    if ($docker) { return $docker.Source }

    $paths = @(
        "C:\Program Files\Docker\Docker\resources\bin\docker.exe",
        "$env:ProgramFiles\Docker\Docker\resources\bin\docker.exe",
        "$env:LOCALAPPDATA\Docker\resources\bin\docker.exe"
    )

    foreach ($p in $paths) {
        if (Test-Path $p) { return $p }
    }

    return $null
}

function Prompt-DockerInstall {
    Write-Log "Docker Desktop not found."

    Add-Type -AssemblyName System.Windows.Forms -ErrorAction SilentlyContinue

    $msg = @"
Docker Desktop is required for file conversion (msConvertR) and peak integration (PeakForgeR).

These features use ProteoWizard and Skyline Docker containers.

Note: Docker Desktop is FREE for personal use, education, and small businesses.
Enterprise use requires a paid subscription.

The batch correction (batchCorrectR) and quality control (qcCheckR) features
work WITHOUT Docker.

Would you like to open the Docker Desktop download page?
"@

    $result = [System.Windows.Forms.MessageBox]::Show(
        $msg,
        "MStargetR - Docker Desktop",
        [System.Windows.Forms.MessageBoxButtons]::YesNo,
        [System.Windows.Forms.MessageBoxIcon]::Information
    )

    if ($result -eq [System.Windows.Forms.DialogResult]::Yes) {
        Start-Process "https://www.docker.com/products/docker-desktop/"
        Write-Log "Opened Docker Desktop download page."
    } else {
        Write-Log "User skipped Docker installation."
    }
}

# ---------------------------------------------------------------------------
# Pandoc Installation (required for HTML report generation)
# ---------------------------------------------------------------------------

function Find-Pandoc {
    $pandoc = Get-Command "pandoc" -ErrorAction SilentlyContinue
    if ($pandoc) { return $pandoc.Source }

    $paths = @(
        "$env:LOCALAPPDATA\Pandoc\pandoc.exe",
        "C:\Program Files\Pandoc\pandoc.exe",
        "$env:ProgramFiles\Pandoc\pandoc.exe"
    )

    foreach ($p in $paths) {
        if (Test-Path $p) { return $p }
    }

    return $null
}

function Install-Pandoc {
    Write-Log "Detecting latest Pandoc version..."

    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

    $pandoc_version = $null
    try {
        $release = Invoke-RestMethod -Uri "https://api.github.com/repos/jgm/pandoc/releases/latest" -UseBasicParsing
        if ($release.tag_name) {
            $pandoc_version = $release.tag_name
        }
    } catch {
        Write-Log "WARNING: Could not detect latest Pandoc version from GitHub."
    }

    if (-not $pandoc_version) {
        $pandoc_version = "3.9.0.2"
        Write-Log "Using fallback Pandoc version: $pandoc_version"
    } else {
        Write-Log "Latest Pandoc version detected: $pandoc_version"
    }

    $pandoc_url = "https://github.com/jgm/pandoc/releases/download/$pandoc_version/pandoc-$pandoc_version-windows-x86_64.msi"
    $pandoc_installer = Join-Path $env:TEMP "pandoc-$pandoc_version-windows-x86_64.msi"

    try {
        Write-Log "Downloading Pandoc $pandoc_version..."
        Invoke-WebRequest -Uri $pandoc_url -OutFile $pandoc_installer -UseBasicParsing

        Write-Log "Installing Pandoc $pandoc_version (silent)..."
        $proc = Start-Process -FilePath "msiexec.exe" -ArgumentList "/i `"$pandoc_installer`" /qn ALLUSERS=1" -Wait -PassThru

        if ($proc.ExitCode -eq 0) {
            Write-Log "Pandoc $pandoc_version installed successfully."

            # Add to PATH for this session
            $pandocBin = "C:\Program Files\Pandoc"
            if (Test-Path $pandocBin) {
                $env:PATH = "$pandocBin;$env:PATH"
            }
        } else {
            # msiexec may require admin; try user-level install
            Write-Log "System-level install returned exit code $($proc.ExitCode), trying user-level..."
            $userPandoc = Join-Path $env:LOCALAPPDATA "Pandoc"
            $proc2 = Start-Process -FilePath "msiexec.exe" -ArgumentList "/i `"$pandoc_installer`" /qn INSTALLDIR=`"$userPandoc`"" -Wait -PassThru
            if ($proc2.ExitCode -eq 0) {
                Write-Log "Pandoc installed to $userPandoc"
                $env:PATH = "$userPandoc;$env:PATH"
            } else {
                Write-Log "WARNING: Pandoc installation failed. HTML reports will be unavailable."
                Write-Log "Install manually from https://pandoc.org/installing.html"
            }
        }
    } catch {
        Write-Log "ERROR: Failed to download/install Pandoc: $_"
        Write-Log "Install manually from https://pandoc.org/installing.html"
    } finally {
        if (Test-Path $pandoc_installer) {
            Remove-Item $pandoc_installer -Force -ErrorAction SilentlyContinue
        }
    }
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

Write-Log "=== MStargetR Prerequisite Check ==="
Write-Log "InstallR=$InstallR, InstallDocker=$InstallDocker"

# Check R
$r_path = Find-R
if ($r_path) {
    Write-Log "R found at: $r_path"
} else {
    Write-Log "R not found on this system."
    if ($InstallR -eq "true") {
        Install-R
        $r_path = Find-R
        if ($r_path) {
            Write-Log "R is now available at: $r_path"
        } else {
            Write-Log "WARNING: R installation may have succeeded but Rscript not found in PATH."
        }
    } else {
        Write-Log "Skipping R installation (not selected)."
    }
}

# Check Docker
$docker_path = Find-Docker
if ($docker_path) {
    Write-Log "Docker found at: $docker_path"
    # Check if daemon is running
    try {
        $info = & docker info 2>&1
        if ($LASTEXITCODE -eq 0) {
            Write-Log "Docker daemon is running."
        } else {
            Write-Log "Docker is installed but the daemon is not running."
        }
    } catch {
        Write-Log "Docker is installed but could not check daemon status."
    }
} else {
    if ($InstallDocker -eq "true") {
        Prompt-DockerInstall
    } else {
        Write-Log "Docker not found. Skipping (not selected)."
    }
}

# Check Pandoc
$pandoc_path = Find-Pandoc
if ($pandoc_path) {
    Write-Log "Pandoc found at: $pandoc_path"
} else {
    Write-Log "Pandoc not found. Installing (required for HTML report generation)..."
    Install-Pandoc
    $pandoc_path = Find-Pandoc
    if ($pandoc_path) {
        Write-Log "Pandoc is now available at: $pandoc_path"
    } else {
        Write-Log "WARNING: Pandoc not found after install. HTML reports will be unavailable."
        Write-Log "Other features (XLSX export, qs2 export) are unaffected."
    }
}

Write-Log "=== Prerequisite check complete ==="
