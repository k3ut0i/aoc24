"
A parsing/map-reduce problem.
"
Class {
	#name : 'Day03',
	#superclass : 'AocSol',
	#traits : 'Runnable',
	#classTraits : 'Runnable classTrait',
	#instVars : [
		'data'
	],
	#category : 'AOC-24-Solutions',
	#package : 'AOC-24',
	#tag : 'Solutions'
}

{ #category : 'helpers' }
Day03 class >> extractNum: mulString [
	| stripped nums |
	stripped := (mulString withoutPrefix: 'mul(') withoutSuffix: ')'.
	nums := (stripped splitOn: ',') collect: [:s | s asNumber ].
	self assert: [ nums size = 2 ].
	^ (nums at: 1) * (nums at: 2).
]

{ #category : 'helpers' }
Day03 class >> getMul: aString [
	| rx |
	rx := 'mul\(\d+,\d+\)|do|don''t'.
	^ rx asRegex matchesIn: aString.
]

{ #category : 'testing' }
Day03 class >> isMul: aString [
	^ 'mul(' = (aString contractTo: 4).
]

{ #category : 'instance creation' }
Day03 >> initFrom: fileNameOrInput [
	data := self class getMul: (fileNameOrInput asFileReference contents).
]

{ #category : 'running' }
Day03 >> part1 [
	| sum |
	sum := 0.
	data do: [ :x | sum := sum + ((self class isMul: x) ifTrue: [ self class extractNum: x ] ifFalse: [ 0 ]) ].
	^ sum.
]

{ #category : 'running' }
Day03 >> part2 [
	| sum factor |
	sum := 0.
	factor := 1.
	data do: [ :x | sum := sum + (factor * ((self class isMul: x)
														ifTrue: [ self class extractNum: x ]
														ifFalse: [ factor := x = 'do'
																				ifTrue: [ 1 ]
																				ifFalse: [ 0 ].
																	 0])) ].
	^ sum.
]
