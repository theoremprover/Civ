SET WORKDIR=%~dp0
cd %WORKDIR%

where plink.exe > tmpFile
set /p GIT_SSH= < tmpFile
del tmpFile

git commit -m "No Message"
if errorlevel 1 goto :err
git push
if errorlevel 1 goto :err
exit 0

:err
pause
exit
