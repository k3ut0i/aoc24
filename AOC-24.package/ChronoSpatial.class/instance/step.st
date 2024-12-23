stepping
step
	| d op |
	d := Dictionary 
			newFrom: { 0 -> #adv.  1 -> #bxl. 
						2 -> #bst.  3 -> #jnz.
						4 -> #bxc.  5 -> #out.
						6 -> #bdv.  7 -> #cdv.}.
	op := d at: self currentOp.
	self perform: op.
	op = #jnz ifFalse: [ ip := ip + 2 ].