CppApplication {
	name: 'task'

	files: ['*.cpp', '*.cppm']

	cpp.cxxLanguageVersion: 'c++23'
	cpp.forceUseCxxModules: true

	// Didn't work without manually settings the paths
	cpp.systemIncludePaths: {
		var l = [
			"/opt/homebrew/opt/llvm/include/c++/v1/",
			"/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include",
		]
		return l.concat(base)
	}
	cpp.libraryPaths: base.concat([
        "/opt/homebrew/opt/llvm/lib/c++",
        "/opt/homebrew/opt/llvm/lib",
		"/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib",
    ])
}
