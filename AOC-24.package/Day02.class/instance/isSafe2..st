running
isSafe2: levelIndex

	| negCount posCount negIndex posIndex zeroIndex zeroCount level errCount |
	negCount := 0.
	negIndex := 0.
	posCount := 0.
	posIndex := 0.
	zeroCount := 0.
	zeroIndex := 0.
	level := diffData at: levelIndex.
	level withIndexDo: [ :e :i |
		e > 3
			ifTrue: [
				posCount := posCount + 1.
				posIndex := i ]
			ifFalse: [
				e < -3
					ifTrue: [
						negCount := negCount + 1.
						negIndex := i ]
					ifFalse: [
						e = 0 ifTrue: [
							zeroCount := zeroCount + 1.
							zeroIndex := i ] ] ] ].
	errCount := negCount + posCount + zeroCount.
	errCount = 0 ifTrue: [ ^ true ].
	errCount > 1 ifTrue: [ ^ false ].
	errCount = 1 ifTrue: [
		| i |
		i := {
			     negIndex.
			     posIndex.
			     zeroIndex } detect: [ :x | x ~= 0 ] 
		"Now again I have to look at boundary conditions and positive and negative cases."]
		