accessing
wordAt: aPoint inDir: aDir

	^ (0 to: 3) collect: [ :i | self at: (aPoint + (i * aDir)) ]
