SET WORKDIR=%~dp0
cd %WORKDIR%
if errorlevel 1 goto :err
git commit -m "No Message"
if errorlevel 1 goto :err
git push
if errorlevel 1 goto :err
exit 0

:err
pause
exit
