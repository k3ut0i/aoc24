running
part1
	| count |
	count := 0.
	1 to: xlen do: [ :x |
		1 to: ylen do: [ :y |
			| p |
			p := x @ y.
			(self at: p) = $X ifTrue: [ count := count + (self countXmas: p) ]
			 ]
		 ].
	^ count.