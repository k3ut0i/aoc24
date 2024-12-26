Class {
	#name : 'Day01Test',
	#superclass : 'AocTest',
	#category : 'AOC-24-Tests',
	#package : 'AOC-24-Tests'
}

{ #category : 'running' }
Day01Test >> setUp [
	| fileOrString |
	super setUp.

	"Put here a common initialization logic for tests"
	fileOrString := directory , 'day01'.
	dayinstance := Day01 new initFrom: fileOrString.
]

{ #category : 'as yet unclassified' }
Day01Test >> testPart1 [
	self assert: (dayinstance part1) equals: 1830467.
]

{ #category : 'as yet unclassified' }
Day01Test >> testPart2 [
	self assert: (dayinstance part2) equals: 26674158.
]
