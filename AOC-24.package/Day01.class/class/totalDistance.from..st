accessing
totalDistance: a from: b
	| d |
	a sort.
	b sort.
	d := a with: b collect: [ :x :y | (x - y) abs ].
	^ d reduce: [ :x :y | x + y ]