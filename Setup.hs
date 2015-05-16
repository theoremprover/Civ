import Distribution.Simple
import Distribution.Simple.Utils (info)
import Distribution.Simple.PreProcess
import Distribution.PackageDescription (emptyHookedBuildInfo)
import Data.List.Utils
import Data.Time
import System.Exit
import System.Process

main = defaultMainWithHooks $ simpleUserHooks {
	hookedPreProcessors = [("preversion",preversionPreprocessor)],
	preBuild = preBuildHook,
	postBuild = postBuildHook }

preversionPreprocessor buildinfo localbuildinfo = PreProcessor {
	platformIndependent = True,
	runPreProcessor = mkSimplePreProcessor $ \ inFile outFile verbosity -> do
		info verbosity $ "Preprocessing " ++ inFile ++ " ..."
		preprocessPreversion inFile outFile
		info verbosity $ "OK, wrote " ++ outFile
		return () }

preprocessPreversion infile outfile = do
	timezone <- getCurrentTimeZone
	curtime <- getCurrentTime
	let compdate = utcToLocalTime timezone curtime
	
	(ExitSuccess,githash,[]) <- readProcessWithExitCode "git" [ "log", "--pretty=format:'%h'", "-n 1" ] ""

	file0 <- readFile infile
	writeFile outfile $ foldl (\ s (old,new) -> replace old new s) file0 $ [
		("<compilationDateString>",show compdate),
		("<gitHash>",head $ lines githash) ]

preBuildHook args configflags = do
	ExitSuccess <- system "touch Version.preversion"
	return emptyHookedBuildInfo

postBuildHook args buildflags packagedesc localbuildinfo = do
	ExitSuccess <- system "rm -f state/CivState/open.lock"
	return ()
