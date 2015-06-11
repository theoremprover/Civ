SET WORKDIR=%~dp0
cd %WORKDIR%
if errorlevel 1 goto :err
git pull origin
if errorlevel 1 goto :err
exit 0

:err
pause
exit
