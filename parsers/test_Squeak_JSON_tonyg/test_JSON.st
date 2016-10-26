| exitCode |
exitCode := 2.
[[[
	FileStream
		readOnlyFileNamed: Smalltalk commandLine arguments first
		do: [:strm | Json readFrom: strm. exitCode := 0].
] on: JsonSyntaxError do: [exitCode := 1]
] on: Error do: [exitCode := 3]
] ensure: [Smalltalk snapshot: false andQuitWithExitCode: exitCode].
