running
part1
	| sum |
	sum := 0.
	data do: [ :x | sum := sum + ((self class isMul: x) ifTrue: [ self class extractNum: x ] ifFalse: [ 0 ]) ].
	^ sum.