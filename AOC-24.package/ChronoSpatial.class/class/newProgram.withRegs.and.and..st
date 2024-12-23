instance creation
newProgram: aProg withRegs: a and: b and: c
	| p |
	p := self new.
	p rega: a.
	p regb: b.
	p regc: c.
	p program: aProg.
	p ip: 1.
	p outqueue: OrderedCollection new.
	^ p.