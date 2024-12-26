"
An instance of the chronospatial computer

"
Class {
	#name : 'Day17',
	#superclass : 'AocSol',
	#instVars : [
		'comp'
	],
	#category : 'AOC-24-Solutions',
	#package : 'AOC-24',
	#tag : 'Solutions'
}

{ #category : 'accessing' }
Day17 >> comp [

	^ comp
]

{ #category : 'accessing' }
Day17 >> comp: anObject [

	comp := anObject
]

{ #category : 'instance creation' }
Day17 >> initFrom: a [
	
	comp := ChronoSpatial newProgram: (a at: 4) withRegs: (a at: 1) and: (a at: 2) and: (a at: 3).
]

{ #category : 'running' }
Day17 >> part1 [
	comp runProg.
	^ comp outqueue reduce: [ :a :b | a asString, ',', b asString ].
	
]
