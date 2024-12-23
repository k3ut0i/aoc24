primitives
bdv
	| x y |
	y := self comboOperand.
	x := rega bitShift: (-1 * y).
	self regb: x.
	^ x