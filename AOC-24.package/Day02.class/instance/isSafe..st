testing
isSafe: i
	| x1 x2|
	x1 := (data at: i) at: 1.
	x2 := (data at: i) at: 2.
	^ x1 = x2 ifTrue: [ false ] ifFalse: [ x1 > x2 
														ifTrue: [ self at: i between: -3 and: -1 ]
														ifFalse: [ self at: i between: 1 and: 3 ] ]