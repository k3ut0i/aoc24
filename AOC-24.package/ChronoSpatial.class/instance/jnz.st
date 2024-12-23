primitives
jnz
	ip := rega = 0 
				ifFalse: [ 1 + self literalOperand ]
				ifTrue: [ ip + 2 ]