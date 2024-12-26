"
A Graph data structure that uses adjacency list representation.
"
Class {
	#name : 'Graph',
	#superclass : 'Object',
	#traits : 'Runnable',
	#classTraits : 'Runnable classTrait',
	#instVars : [
		'nodes',
		'adjacencyList'
	],
	#category : 'AOC-24-Common',
	#package : 'AOC-24',
	#tag : 'Common'
}

{ #category : 'instance creation' }
Graph >> initFrom: a2d [
	
]

{ #category : 'instance creation' }
Graph >> initFrom: a2d restrict: aBlock hasCorners: aBool [
	"create the graph given a 2d matrix a2d, edges are restricted by a binary aBlock. hasCorners includes diagonals"
	^ self notImplemented.
]
