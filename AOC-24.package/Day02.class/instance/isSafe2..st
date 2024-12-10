running
isSafe2: levelIndex
	| negCount posCount negIndex posIndex level |
	negCount := 0.
	posCount := 0.
	level := diffData at: levelIndex .
	level withIndexDo: [ :e :i |
		e > 0 
			ifTrue: [ posCount := posCount + 1. posIndex := i]
		 	ifFalse: [negCount := negCount + 1. negIndex := i]
			]
	