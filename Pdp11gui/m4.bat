@rem Wrapper for preprocessor M4.
@rem  M4 is used by PDP11GUI to preprocess the machine descriotion files *.ini
@rem
@rem Parameters:
@rem %1 = source file, surrounded by "..."
@rem %2 = output file, surrounded by "..."

@rem "m4.exe" "%1" > "%2"
@rem Search for Libraries in APPDATA, see installer
"%PDP11GUIEXEDIR%\m4.exe" --include="%PDP11GUIAPPDATADIR%\machines" %1 > %2

@rem set PAUSE to debug
@rem pause


