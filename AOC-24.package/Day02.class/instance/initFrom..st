instance creation
initFrom: fname
	| ls |
	ls := self getLines: fname.
	data := ls collect: [ :l | self intsFromString: l ].
	diffData := OrderedCollection new.
	^ self