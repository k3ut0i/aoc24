primitives
adv
	| x y |
	y := self comboOperand.
	x := rega bitShift: (-1 * y).
	self rega: x.
	^ x