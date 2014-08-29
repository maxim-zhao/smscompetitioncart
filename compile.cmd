@echo off

setlocal

set WLAPATH=..\..\c\wla dx\binaries

rem Code and make linkfile
echo [objects]>linkfile
for %%f in (CompetitionCart2.sms.asm Game-*.asm) do (
  echo %%~nxf...
  "%WLAPATH%\wla-z80.exe" -o "%%f" "%%~nf.o"
  if errorlevel 1 goto :eof
  echo %%~nf.o >> linkfile
)

"%WLAPATH%\wlalink.exe" -drvs linkfile CompetitionCart.sms 2>&1 | find /v " has 00000 bytes "
