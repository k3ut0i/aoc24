running
part1
	^ (1 to: data size) count: [ :i | self isSafe: i ].