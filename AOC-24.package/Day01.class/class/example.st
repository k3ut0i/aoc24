examples
example
	| fname d |
	fname := '/home/keutoi/projects/aoc24/resources/day01'.
	d := self new.
	Transcript crShow: (d run: fname).
	^ d run: fname.