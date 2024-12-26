"
Common code for all Aoc problems.
"
Class {
	#name : 'AocSol',
	#superclass : 'Object',
	#category : 'AOC-24-Common',
	#package : 'AOC-24',
	#tag : 'Common'
}

{ #category : 'instance creation' }
AocSol >> initFrom: fileNameOrInput [
	^ self subclassResponsibility .
]

{ #category : 'running' }
AocSol >> part1 [
	^ self subclassResponsibility.
]

{ #category : 'running' }
AocSol >> part2 [
	^ self subclassResponsibility.
]
