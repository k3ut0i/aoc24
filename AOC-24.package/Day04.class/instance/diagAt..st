accessing
diagAt: aPoint
	| w |
	w := DiagV collect: [ :d | self at: (aPoint + d) ].
	^ self stringFromChars: w.