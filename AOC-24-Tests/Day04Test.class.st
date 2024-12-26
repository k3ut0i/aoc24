Class {
	#name : 'Day04Test',
	#superclass : 'AocTest',
	#category : 'AOC-24-Tests',
	#package : 'AOC-24-Tests'
}

{ #category : 'running' }
Day04Test >> setUp [
	super setUp.

	"Put here a common initialization logic for tests"
	directory := '/home/keutoi/projects/aoc24/resources/'.
	dayinstance := Day04 new initFrom: (directory, 'day04test').
]

{ #category : 'tests' }
Day04Test >> testPart1 [
	^ self assert: (dayinstance part1) equals: 18.
]

{ #category : 'tests' }
Day04Test >> testPart2 [
	^ self assert: (dayinstance part2) equals: 9.

]
