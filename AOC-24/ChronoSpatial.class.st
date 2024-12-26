"
A simulation for the chronospatial computer as defined in AOC2024-17.
https://adventofcode.com/2024/day/17

"
Class {
	#name : 'ChronoSpatial',
	#superclass : 'Object',
	#instVars : [
		'rega',
		'regb',
		'regc',
		'ip',
		'outqueue',
		'program'
	],
	#category : 'AOC-24-Specific',
	#package : 'AOC-24',
	#tag : 'Specific'
}

{ #category : 'instance creation' }
ChronoSpatial class >> newProgram: aProg withRegs: a and: b and: c [
	| p |
	p := self new.
	p rega: a.
	p regb: b.
	p regc: c.
	p program: aProg.
	p ip: 1.
	p outqueue: OrderedCollection new.
	^ p.
]

{ #category : 'primitives' }
ChronoSpatial >> adv [
	| x y |
	y := self comboOperand.
	x := rega bitShift: (-1 * y).
	self rega: x.
	^ x
]

{ #category : 'primitives' }
ChronoSpatial >> bdv [
	| x y |
	y := self comboOperand.
	x := rega bitShift: (-1 * y).
	self regb: x.
	^ x
]

{ #category : 'primitives' }
ChronoSpatial >> bst [
	regb := self comboOperand bitAnd: 7
]

{ #category : 'primitives' }
ChronoSpatial >> bxc [
	regb := regc bitXor: regb.
]

{ #category : 'primitives' }
ChronoSpatial >> bxl [
	regb := (regb bitXor: (self literalOperand)) bitAnd: 7.
]

{ #category : 'primitives' }
ChronoSpatial >> cdv [
	| x y |
	y := self comboOperand.
	x := rega bitShift: (-1 * y).
	self regc: x.
	^ x
]

{ #category : 'accessing' }
ChronoSpatial >> comboOperand [
	^ self getVal: (program at: (ip+1)).
]

{ #category : 'accessing' }
ChronoSpatial >> currentOp [
	^ program at: ip
]

{ #category : 'accessing' }
ChronoSpatial >> getVal: comboOp [
   ^ comboOp < 4
		ifTrue: [  comboOp ]
		ifFalse: [ comboOp = 4
						ifTrue: [ rega ]
						ifFalse: [ comboOp = 5
										ifTrue: [ regb ]
										ifFalse: [ comboOp = 6
														ifTrue: [ regc ]
														ifFalse: [ self error: 'comboOp cannot be 7']]]].
]

{ #category : 'accessing' }
ChronoSpatial >> ip: x [
	ip := x
]

{ #category : 'primitives' }
ChronoSpatial >> jnz [
	ip := rega = 0 
				ifFalse: [ 1 + self literalOperand ]
				ifTrue: [ ip + 2 ]
]

{ #category : 'accessing' }
ChronoSpatial >> literalOperand [
	^ program at: (ip+1).
]

{ #category : 'primitives' }
ChronoSpatial >> out [
	outqueue addLast: (self comboOperand bitAnd: 7).
]

{ #category : 'accessing' }
ChronoSpatial >> outqueue [
	^ outqueue.
]

{ #category : 'accessing' }
ChronoSpatial >> outqueue: o [
	outqueue := o
]

{ #category : 'printing' }
ChronoSpatial >> printOn: aStream [
	'A [', rega asString, '] ', 'B [', regb asString, '] ',	 'C [', regc asString, '] ' printOn: aStream.
	aStream lf.
	outqueue printOn: aStream.
]

{ #category : 'accessing' }
ChronoSpatial >> program: aProg [
	program := aProg.
	
]

{ #category : 'accessing' }
ChronoSpatial >> rega: a [
	rega := a.
]

{ #category : 'accessing' }
ChronoSpatial >> regb: b [
	regb := b.
]

{ #category : 'accessing' }
ChronoSpatial >> regc: c [
	regc := c.
	
]

{ #category : 'stepping' }
ChronoSpatial >> runProg [
	[ ip < program size ] whileTrue: [ self step. ].
	^ self
]

{ #category : 'stepping' }
ChronoSpatial >> step [
	| d op |
	d := Dictionary 
			newFrom: { 0 -> #adv.  1 -> #bxl. 
						2 -> #bst.  3 -> #jnz.
						4 -> #bxc.  5 -> #out.
						6 -> #bdv.  7 -> #cdv.}.
	op := d at: self currentOp.
	self perform: op.
	op = #jnz ifFalse: [ ip := ip + 2 ].
]
