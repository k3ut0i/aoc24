accessing
xmasAt: aPoint inDir: aDir
	| s |
	s := self wordAt: aPoint inDir: aDir.
	^ (1 to: 4) allSatisfy: [ :i | (s at: i) = ('XMAS' at: i)].