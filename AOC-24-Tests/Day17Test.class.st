Class {
	#name : 'Day17Test',
	#superclass : 'AocTest',
	#category : 'AOC-24-Tests',
	#package : 'AOC-24-Tests'
}

{ #category : 'running' }
Day17Test >> setUp [
	super setUp.

	"Put here a common initialization logic for tests"
	dayinstance := Day17 new initFrom: #(48744869 0 0 #(2 4 1 2 7 5 1 3 4 4 5 5 0 3 3 0)).
]

{ #category : 'tests' }
Day17Test >> testPart1 [
	self assert: (dayinstance part1) equals: '7,1,5,2,4,0,7,6,1'.
	
]
