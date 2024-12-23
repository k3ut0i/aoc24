running
part1
	comp runProg.
	^ comp outqueue reduce: [ :a :b | a asString, ',', b asString ].
	