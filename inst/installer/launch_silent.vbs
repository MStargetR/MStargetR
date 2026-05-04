' MStargetR Launcher (VBS Wrapper)
' =============================================================================
' This VBScript launches MStargetR via launch.bat.
' Window style 1 = normal window so the user can see progress and errors.
' To run hidden (production mode), change the 1 below to 0.

Dim WshShell, appDir, batFile

Set WshShell = CreateObject("WScript.Shell")

' Get the directory where this script lives
appDir = Left(WScript.ScriptFullName, InStrRev(WScript.ScriptFullName, "\"))
batFile = appDir & "launch.bat"

' Window styles:
'   0 = hidden (no window at all - use only when everything is stable)
'   1 = normal window (recommended - user can see output and errors)
'   7 = minimised, no focus
WshShell.Run """" & batFile & """", 1, False

Set WshShell = Nothing
