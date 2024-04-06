@echo off

if "%~1"=="" goto :done

echo Sign %1 ...

echo "C:\Program Files (x86)\Windows Kits\10\bin\10.0.22000.0\x64\signtool.exe" verify /pa %1
>NUL 2>&1 "C:\Program Files (x86)\Windows Kits\10\bin\10.0.22000.0\x64\signtool.exe" verify /pa %1 && (
  echo File %2 is already signed, skipping
) || (
  "C:\Program Files (x86)\Windows Kits\10\bin\10.0.22000.0\x64\signtool.exe" sign /a /tr http://timestamp.globalsign.com/tsa/r6advanced1 /td SHA256 /fd SHA256 /n "HickelSOFT" /v %1
  exit /b 0
)

:done
