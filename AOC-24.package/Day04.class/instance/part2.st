running
part2
	| count |
	count := 0.
	1 to: xlen do: [ :x |
		1 to: ylen do: [ :y |
			| p |
			p := x @ y.
			(self hasMasAt: p)  ifTrue: [ count := count + 1 ]
			 ]
		 ].
	^ count.