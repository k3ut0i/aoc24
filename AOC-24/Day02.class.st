"
## Part 1
Filtering problem
## Part 2 - Incomplete

"
Class {
	#name : 'Day02',
	#superclass : 'AocSol',
	#traits : 'Runnable',
	#classTraits : 'Runnable classTrait',
	#instVars : [
		'data',
		'diffData'
	],
	#category : 'AOC-24-Solutions',
	#package : 'AOC-24',
	#tag : 'Solutions'
}

{ #category : 'examples' }
Day02 class >> example [
	| fname d |
	fname := '/home/keutoi/projects/aoc24/resources/day02'.
	d := self new.
	d initFrom: fname.
	Transcript show: d part1.
	
]

{ #category : 'accessing' }
Day02 >> at: i between: l and: u [
	^ (diffData at: i) allSatisfy: [ :each | each between: l and: u ]
]

{ #category : 'math' }
Day02 >> createDiffData [ | a b prev | 1 to: data size do: [ :i |
	a := data at: i.
	prev := a at: 1.
	b := a
		     collect: [ :x |
			     | tmp |
			     tmp := prev.
			     prev := x.
			     x - tmp ]
		     from: 2
		     to: a size.
	diffData addLast: b]
]

{ #category : 'instance creation' }
Day02 >> initFrom: fname [
	| ls |
	ls := self getLines: fname.
	data := ls collect: [ :l | self intsFromString: l ].
	diffData := OrderedCollection new.
	self createDiffData.
	^ self
]

{ #category : 'running' }
Day02 >> isSafe2: levelIndex [

	| negCount posCount negIndex posIndex zeroIndex zeroCount level errCount |
	negCount := 0.
	negIndex := 0.
	posCount := 0.
	posIndex := 0.
	zeroCount := 0.
	zeroIndex := 0.
	level := diffData at: levelIndex.
	level withIndexDo: [ :e :i |
		e > 3
			ifTrue: [
				posCount := posCount + 1.
				posIndex := i ]
			ifFalse: [
				e < -3
					ifTrue: [
						negCount := negCount + 1.
						negIndex := i ]
					ifFalse: [
						e = 0 ifTrue: [
							zeroCount := zeroCount + 1.
							zeroIndex := i ] ] ] ].
	errCount := negCount + posCount + zeroCount.
	errCount = 0 ifTrue: [ ^ true ].
	errCount > 1 ifTrue: [ ^ false ].
	errCount = 1 ifTrue: [
		| i |
		i := {
			     negIndex.
			     posIndex.
			     zeroIndex } detect: [ :x | x ~= 0 ] 
		"Now again I have to look at boundary conditions and positive and negative cases."]
		
]

{ #category : 'testing' }
Day02 >> isSafe: i [
	| x1 x2|
	x1 := (data at: i) at: 1.
	x2 := (data at: i) at: 2.
	^ x1 = x2 ifTrue: [ false ] ifFalse: [ x1 > x2 
														ifTrue: [ self at: i between: -3 and: -1 ]
														ifFalse: [ self at: i between: 1 and: 3 ] ]
]

{ #category : 'running' }
Day02 >> part1 [
	^ (1 to: data size) count: [ :i | self isSafe: i ].
]

{ #category : 'running' }
Day02 >> part2 [
	^ (1 to: data size) count: [ :i | self isSafe2: i ].
]
