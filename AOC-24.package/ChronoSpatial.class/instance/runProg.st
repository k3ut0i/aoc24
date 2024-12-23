stepping
runProg
	[ ip < program size ] whileTrue: [ self step. ].
	^ self