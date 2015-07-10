SET WORKDIR=%~dp0
cd %WORKDIR%

where plink.exe > tmpFile
set /p GIT_SSH= < tmpFile
del tmpFile

git commit -a -m "No Message"
echo %errorlevel%
pause
exit
if errorlevel 1 goto :err
git push origin master:master
if errorlevel 1 goto :err
exit 0

:err
pause
exit
