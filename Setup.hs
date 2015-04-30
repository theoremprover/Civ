import Distribution.Simple
import Distribution.PackageDescription (emptyHookedBuildInfo)
import System.Process

main = defaultMainWithHooks $ simpleUserHooks {
	preConf = preconf }

preconf = args configflags = do
	system "./updateversion.sh"
	return emptyHookedBuildInfo
