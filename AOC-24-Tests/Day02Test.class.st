Class {
	#name : 'Day02Test',
	#superclass : 'AocTest',
	#category : 'AOC-24-Tests',
	#package : 'AOC-24-Tests'
}

{ #category : 'running' }
Day02Test >> setUp [
	super setUp.

	"Put here a common initialization logic for tests"
	dayinstance := Day02 new initFrom: (directory, 'day02').
]

{ #category : 'tests' }
Day02Test >> testPart1 [
	self assert: (dayinstance part1) equals: 257.
]
