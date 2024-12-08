running
run: filenameOrInput
	| d |
	d := self initFrom: filenameOrInput.
	^ 'Part1: ', (d part1 asString), ' Part2: ', (d part2 asString)