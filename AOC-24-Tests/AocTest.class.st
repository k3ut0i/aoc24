Class {
	#name : 'AocTest',
	#superclass : 'TestCase',
	#instVars : [
		'dayinstance'
	],
	#classVars : [
		'directory'
	],
	#category : 'AOC-24-Tests',
	#package : 'AOC-24-Tests'
}

{ #category : 'testing' }
AocTest class >> isAbstract [

	^ self == AocTest
]

{ #category : 'running' }
AocTest >> setUp [
	super setUp.

	"Put here a common initialization logic for tests"
	directory := '/home/keutoi/projects/aoc24/resources/'
]

{ #category : 'tests' }
AocTest >> testPart1 [
	^ self subclassResponsibility.
]

{ #category : 'tests' }
AocTest >> testPart2 [
	^ self subclassResponsibility.
]
