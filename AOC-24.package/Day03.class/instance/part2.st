running
part2
	| sum factor |
	sum := 0.
	factor := 1.
	data do: [ :x | sum := sum + (factor * ((self class isMul: x)
														ifTrue: [ self class extractNum: x ]
														ifFalse: [ factor := x = 'do'
																				ifTrue: [ 1 ]
																				ifFalse: [ 0 ].
																	 0])) ].
	^ sum.