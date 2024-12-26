"
## Day04 - Ceres Search
A simple linear search.
"
Class {
	#name : 'Day04',
	#superclass : 'AocSol',
	#traits : 'Runnable + StringManip',
	#classTraits : 'Runnable classTrait + StringManip classTrait',
	#instVars : [
		'maze',
		'xlen',
		'ylen'
	],
	#classVars : [
		'DiagV',
		'DirV',
		'PossiblePatterns'
	],
	#category : 'AOC-24-Solutions',
	#package : 'AOC-24',
	#tag : 'Solutions'
}

{ #category : 'class initialization' }
Day04 class >> initialize [
	DirV := {-1@ -1. -1@0. -1@1. 0@ -1. 0@1. 1@ -1. 1@0. 1@1}.
	DiagV := { -1@ -1. 1@ -1. 1@1. -1@1 }.
	PossiblePatterns := { 'MMSS'. 'MSSM'. 'SMMS'. 'SSMM' }.
]

{ #category : 'accessing' }
Day04 >> at: aPoint [
	^ self x: (aPoint x) y: (aPoint y).
]

{ #category : 'as yet unclassified' }
Day04 >> countXmas: aPoint [
	| count |
	count := 0.
	DirV do: [ :d | (self xmasAt: aPoint inDir: d) ifTrue: [ count := count + 1 ] ].
	^ count.
]

{ #category : 'accessing' }
Day04 >> diagAt: aPoint [
	| w |
	w := DiagV collect: [ :d | self at: (aPoint + d) ].
	^ self stringFromChars: w.
]

{ #category : 'accessing' }
Day04 >> hasMasAt: aPoint [
	| w |
	w := self diagAt: aPoint.
	^ ($A = (self at: aPoint)) ifTrue: [ PossiblePatterns anySatisfy: [ :p | p = w ] ] ifFalse: [ false ].
]

{ #category : 'instance creation' }
Day04 >> initFrom: fileNameOrInput [
	| ls |
	ls := self getLines: fileNameOrInput.
	ylen := ls size.
	xlen := (ls at: 1) size.
	maze := Array withAll: ls.	
]

{ #category : 'running' }
Day04 >> part1 [
	| count |
	count := 0.
	1 to: xlen do: [ :x |
		1 to: ylen do: [ :y |
			| p |
			p := x @ y.
			(self at: p) = $X ifTrue: [ count := count + (self countXmas: p) ]
			 ]
		 ].
	^ count.
]

{ #category : 'running' }
Day04 >> part2 [
	| count |
	count := 0.
	1 to: xlen do: [ :x |
		1 to: ylen do: [ :y |
			| p |
			p := x @ y.
			(self hasMasAt: p)  ifTrue: [ count := count + 1 ]
			 ]
		 ].
	^ count.
]

{ #category : 'accessing' }
Day04 >> wordAt: aPoint inDir: aDir [

	^ (0 to: 3) collect: [ :i | self at: (aPoint + (i * aDir)) ]

]

{ #category : 'accessing - array' }
Day04 >> x: xc y: yc [
	^ ({ (0 < xc). (0 < yc). (xc <= xlen). (yc <= ylen) } allSatisfy: [ :b | b ])
			ifTrue: [ (maze at: yc) at: xc ]
			ifFalse: [ $0 ]
]

{ #category : 'accessing' }
Day04 >> xmasAt: aPoint inDir: aDir [
	| s |
	s := self wordAt: aPoint inDir: aDir.
	^ (1 to: 4) allSatisfy: [ :i | (s at: i) = ('XMAS' at: i)].
]
