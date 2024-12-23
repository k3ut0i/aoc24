running
part1
	self runProg.
	^ outqueue reduce: [ :a :b | a asString, ',', b asString ].
	