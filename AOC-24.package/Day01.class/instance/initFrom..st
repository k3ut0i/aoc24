instance creation
initFrom: filename
	| ls |
	ls := self getLines: filename.
	a := OrderedCollection new.
	b := OrderedCollection new.
	"Transcript nextPut: ls."
	ls select: [ :l | l isNotEmpty ] thenDo:
	 [ :l |
		|x| 
		x := self intsFromString: l.
		a add: (x at: 1).
		b add: (x at: 2)
		 ].
	^ self