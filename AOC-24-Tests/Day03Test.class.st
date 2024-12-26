Class {
	#name : 'Day03Test',
	#superclass : 'AocTest',
	#category : 'AOC-24-Tests',
	#package : 'AOC-24-Tests'
}

{ #category : 'running' }
Day03Test >> setUp [
	super setUp.

	"Put here a common initialization logic for tests"
	dayinstance := Day03 new initFrom: (directory,'day03').
	
]

{ #category : 'tests' }
Day03Test >> testPart1 [
	self assert: (dayinstance part1) equals: 187825547.
]

{ #category : 'tests' }
Day03Test >> testPart2 [
	self assert: (dayinstance part2) equals: 85508223.
]
