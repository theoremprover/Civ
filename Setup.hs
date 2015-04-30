import Distribution.Simple
import Distribution.PackageDescription (emptyHookedBuildInfo)

main = defaultMainWithHooks $ simpleUserHooks {
	preBuild = prebuild }

prebuild args buildflags = do
	system "pwd"
	return emptyHookedBuildInfo
