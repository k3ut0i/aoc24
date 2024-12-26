as yet unclassified
countXmas: aPoint
	| count |
	count := 0.
	DirV do: [ :d | (self xmasAt: aPoint inDir: d) ifTrue: [ count := count + 1 ] ].
	^ count.