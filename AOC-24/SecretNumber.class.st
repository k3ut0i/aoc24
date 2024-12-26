"
A number with arithmetic defined in AOC2024-22.
"
Class {
	#name : 'SecretNumber',
	#superclass : 'SmallInteger',
	#category : 'AOC-24-Specific',
	#package : 'AOC-24',
	#tag : 'Specific'
}

{ #category : 'arithmetic' }
SecretNumber >> mix: aVal [
	 ^ self bitXor: aVal.
]

{ #category : 'arithmetic' }
SecretNumber >> next [
	| s1 s2 s3 |
	s1 := (self mix: self * 64) prune.
	s2 := (self mix: s1 // 32) prune.
	s3 := (self mix: s2 * 2048) prune.
	^ s3.
]

{ #category : 'arithmetic' }
SecretNumber >> prune [
	^ self \\ 16777216.
]
