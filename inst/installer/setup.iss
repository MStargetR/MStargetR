; MStargetR Windows Installer - Inno Setup Script
; =============================================================================
; Compile with Inno Setup 6.x: https://jrsoftware.org/isinfo.php
;
; This installer:
;   1. Checks for R (>= 4.1.0) and offers to download/install if missing
;   2. Checks for Docker Desktop and prompts if missing
;   3. Installs MStargetR and all R package dependencies
;   4. Creates Start Menu and Desktop shortcuts
;   5. Registers an uninstaller
;
; To build: Open this file in Inno Setup Compiler and click Build > Compile.
; Output: MStargetR_Setup.exe in the Output/ directory.

#define MyAppName "MStargetR"
#define MyAppVersion "1.3.0"
#define MyAppPublisher "Harrison Szemray"
#define MyAppURL "https://github.com/MStargetR/MStargetR"
#define MyAppExeName "MStargetR.bat"

[Setup]
AppId={{E8F2A1B3-5C7D-4E9F-B6A8-1D3E5F7A9B2C}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}/issues
DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
OutputDir=Output
OutputBaseFilename=MStargetR_Setup
SetupIconFile=assets\mstargetr.ico
UninstallDisplayIcon={app}\mstargetr.ico
Compression=lzma2
SolidCompression=yes
WizardStyle=modern
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
LicenseFile=..\..\LICENSE.md
MinVersion=10.0

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"
Name: "installr"; Description: "Download and install R (if not found)"; GroupDescription: "Prerequisites"
Name: "installdocker"; Description: "Prompt to install Docker Desktop (if not found)"; GroupDescription: "Prerequisites"

[Files]
; Core launcher and scripts
Source: "launch.bat"; DestDir: "{app}"; Flags: ignoreversion
Source: "launch_app.R"; DestDir: "{app}"; Flags: ignoreversion
Source: "launch_silent.vbs"; DestDir: "{app}"; Flags: ignoreversion
Source: "install_deps.R"; DestDir: "{app}"; Flags: ignoreversion
Source: "check_prerequisites.ps1"; DestDir: "{app}"; Flags: ignoreversion
Source: "uninstall_cleanup.bat"; DestDir: "{app}"; Flags: ignoreversion
; Application icon
Source: "assets\mstargetr.ico"; DestDir: "{app}"; Flags: ignoreversion
; Bundled R package source (allows offline install of MStargetR itself)
Source: "..\..\DESCRIPTION"; DestDir: "{app}\source"; Flags: ignoreversion
Source: "..\..\NAMESPACE"; DestDir: "{app}\source"; Flags: ignoreversion
Source: "..\..\R\*"; DestDir: "{app}\source\R"; Flags: ignoreversion recursesubdirs
Source: "..\..\inst\shiny\*"; DestDir: "{app}\source\inst\shiny"; Flags: ignoreversion recursesubdirs
Source: "..\..\inst\templates\*"; DestDir: "{app}\source\inst\templates"; Flags: ignoreversion recursesubdirs
Source: "..\..\inst\scripts\*"; DestDir: "{app}\source\inst\scripts"; Flags: ignoreversion recursesubdirs
Source: "..\..\man\*"; DestDir: "{app}\source\man"; Flags: ignoreversion recursesubdirs
Source: "..\..\inst\extdata\*"; DestDir: "{app}\source\inst\extdata"; Flags: ignoreversion recursesubdirs
Source: "..\..\inst\rmd\*"; DestDir: "{app}\source\inst\rmd"; Flags: ignoreversion recursesubdirs

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\launch_silent.vbs"; \
  IconFilename: "{app}\mstargetr.ico"; Comment: "Launch MStargetR GUI"
Name: "{group}\Uninstall {#MyAppName}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\launch_silent.vbs"; \
  IconFilename: "{app}\mstargetr.ico"; Tasks: desktopicon; Comment: "Launch MStargetR GUI"

[Run]
; Step 1: Check and install prerequisites (visible so user sees progress)
Filename: "powershell.exe"; \
  Parameters: "-ExecutionPolicy Bypass -File ""{app}\check_prerequisites.ps1"" -InstallR {code:ShouldInstallR} -InstallDocker {code:ShouldInstallDocker}"; \
  StatusMsg: "Checking prerequisites..."; \
  Flags: waituntilterminated

; Step 2: Install R packages from bundled source (visible so user sees progress)
Filename: "powershell.exe"; \
  Parameters: "-ExecutionPolicy Bypass -Command ""& {{ $rpath = (Get-Command Rscript -ErrorAction SilentlyContinue).Source; if (-not $rpath) {{ $rdir = Get-ChildItem 'C:\Program Files\R' -Directory -ErrorAction SilentlyContinue | Sort-Object Name -Descending | Select-Object -First 1; if ($rdir) {{ $rpath = Join-Path $rdir.FullName 'bin\Rscript.exe' }} }}; if (-not $rpath) {{ $rlocal = Get-ChildItem ""$env:LOCALAPPDATA\Programs\R"" -Directory -ErrorAction SilentlyContinue | Sort-Object Name -Descending | Select-Object -First 1; if ($rlocal) {{ $rpath = Join-Path $rlocal.FullName 'bin\Rscript.exe' }} }}; if ($rpath -and (Test-Path $rpath)) {{ & $rpath '""{app}\install_deps.R"" --source-dir ""{app}\source""' }} else {{ Write-Host 'WARNING: Rscript not found. Please install R packages manually.' }} }}"""; \
  StatusMsg: "Installing R packages (this may take several minutes)..."; \
  Flags: waituntilterminated

; Step 3: Launch the app after install
Filename: "{app}\launch_silent.vbs"; \
  Description: "Launch {#MyAppName} now"; \
  Flags: postinstall nowait shellexec skipifsilent

[UninstallRun]
Filename: "{app}\uninstall_cleanup.bat"; RunOnceId: "MStargetRCleanup"; Flags: runhidden waituntilterminated

[Code]
// Pascal Script functions for conditional task execution

function ShouldInstallR(Param: String): String;
begin
  if WizardIsTaskSelected('installr') then
    Result := 'true'
  else
    Result := 'false';
end;

function ShouldInstallDocker(Param: String): String;
begin
  if WizardIsTaskSelected('installdocker') then
    Result := 'true'
  else
    Result := 'false';
end;

// Check if R is installed
function IsRInstalled(): Boolean;
var
  ResultCode: Integer;
begin
  Result := Exec('cmd.exe', '/c where Rscript >nul 2>&1', '', SW_HIDE, ewWaitUntilTerminated, ResultCode)
            and (ResultCode = 0);
  if not Result then
  begin
    // Check default install locations
    Result := DirExists('C:\Program Files\R')
           or DirExists(ExpandConstant('{localappdata}\Programs\R'));
  end;
end;

// Check if Docker is installed
function IsDockerInstalled(): Boolean;
var
  ResultCode: Integer;
begin
  Result := Exec('cmd.exe', '/c where docker >nul 2>&1', '', SW_HIDE, ewWaitUntilTerminated, ResultCode)
            and (ResultCode = 0);
end;

// Pre-select tasks based on what's already installed
procedure CurPageChanged(CurPageID: Integer);
var
  I: Integer;
begin
  if CurPageID = wpSelectTasks then
  begin
    for I := 0 to WizardForm.TasksList.Items.Count - 1 do
    begin
      if IsRInstalled() and (Pos('install R', WizardForm.TasksList.ItemCaption[I]) > 0) then
        WizardForm.TasksList.Checked[I] := False;
      if IsDockerInstalled() and (Pos('install Docker', WizardForm.TasksList.ItemCaption[I]) > 0) then
        WizardForm.TasksList.Checked[I] := False;
    end;
  end;
end;

// Show info page about what will be installed
function InitializeSetup(): Boolean;
begin
  Result := True;
end;
