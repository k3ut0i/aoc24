running
part2
	| runA dict |
	"needs sorted _b_ so must run this after part1."
	runA := RunArray newFromArray: b.
	dict := Dictionary new.
	runA runsAndValuesDo: [ :run :value | dict at: value put: run].
	^ (a collect: [ :each | each * (dict at: each ifAbsent: 0) ]) sum.