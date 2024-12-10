io-string
intsFromString: s
	| ss |
	ss := Character space split: s.
	^ ss select: [ :x | x isNotEmpty ] thenCollect: [ :x | Integer readFrom: x ]