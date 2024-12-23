primitives
cdv
	| x y |
	y := self comboOperand.
	x := rega bitShift: (-1 * y).
	self regc: x.
	^ x