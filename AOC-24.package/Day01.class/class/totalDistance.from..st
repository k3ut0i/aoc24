accessing
totalDistance: a from: b
	| d |
	d := a with: b collect: [ :x :y | (x - y) abs ].
	^ d reduce: [ :x :y | x + y ]