"
a simple zipWith(with:collect:), reduce.
For some reason, I implemented the actual function as class side method and used it on the slot variables in instance side method.
"
Class {
	#name : 'Day01',
	#superclass : 'AocSol',
	#traits : 'Runnable',
	#classTraits : 'Runnable classTrait',
	#instVars : [
		'a',
		'b'
	],
	#category : 'AOC-24-Solutions',
	#package : 'AOC-24',
	#tag : 'Solutions'
}

{ #category : 'examples' }
Day01 class >> example [
	| fname d |
	fname := '/home/keutoi/projects/aoc24/resources/day01'.
	d := self new.
	Transcript crShow: (d run: fname).
	^ d run: fname.
]

{ #category : 'accessing' }
Day01 class >> totalDistance: a from: b [
	| d |
	d := a with: b collect: [ :x :y | (x - y) abs ].
	^ d reduce: [ :x :y | x + y ]
]

{ #category : 'instance creation' }
Day01 >> initFrom: filename [
	| ls |
	ls := self getLines: filename.
	a := OrderedCollection new.
	b := OrderedCollection new.
	"Transcript nextPut: ls."
	ls select: [ :l | l isNotEmpty ] thenDo:
	 [ :l |
		|x| 
		x := self intsFromString: l.
		a add: (x at: 1).
		b add: (x at: 2)
		 ].
	a sort.
	b sort.
	^ self
]

{ #category : 'running' }
Day01 >> part1 [
	^ self totalDistance
]

{ #category : 'running' }
Day01 >> part2 [
	| runA dict |
	"needs sorted _b_ so must run this after part1."
	runA := RunArray newFromArray: b.
	dict := Dictionary new.
	runA runsAndValuesDo: [ :run :value | dict at: value put: run].
	^ (a collect: [ :each | each * (dict at: each ifAbsent: 0) ]) sum.
]

{ #category : 'accessing' }
Day01 >> totalDistance [
	^ self class totalDistance: a from: b
]
