rem Wrapper for macro11.exe or any other MACRO-11 assembler
rem
rem Parameters:
rem %1 = source file
rem %2 = listing file

rem The output of MACRO-11 is an object module that must be linked before it can be loaded.
rem Macro11.exe has no linking step, so ONLY ABSOLUTE SYMBOLS are allowed in the code!
rem A relocatable symbol (= relative to module start) is marked with an single quote '
rem See pages 3-15, 4-1 and 6-14 of "PDP-11 MACRO-11 Langugage Reference Manual (Version 5.5, Oct 1987, AA-KX10A-TC).pdf"


rem I use modified http://www.dbit.com/pub/pdp11/macro11/
rem "-e AMA" = ".ENABL AMA" = assemble relocatable addrs as absolute, since we have no linker
rem "-e LISTHEX = list binary code in hex notation (instead of octal)

rem "%PDP11GUIEXEDIR%\macro11.exe" -e AMA %1 -l %2
"%PDP11GUIEXEDIR%\macro11.exe" %1 -l %2

rem generate a listing with binary code in hex notation
rem  (support for logic analyzer evauluation)
"%PDP11GUIEXEDIR%\macro11.exe" -e listhex %1 -l %2.hex


rem pause

