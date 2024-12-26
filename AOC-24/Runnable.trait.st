"
Almost all instance of problems take an input and give and ouput after running them. This trait should codify that behaviour. I want to avoid the rewriting of this every day.
"
Trait {
	#name : 'Runnable',
	#category : 'AOC-24-Common',
	#package : 'AOC-24',
	#tag : 'Common'
}

{ #category : 'examples' }
Runnable classSide >> example [
	^ self notImplemented
]

{ #category : 'io-string' }
Runnable >> getLines: filename [
	| f lines |
	f := File named: filename asFileReference fullName.
	lines := f readStreamDo: [ :s | Character lf split: s upToEnd asString ].
	((lines last) isAllSeparators) ifTrue: [ lines removeLast ].
	^ lines
]

{ #category : 'io-string' }
Runnable >> intsFromString: s [
	| ss |
	ss := Character space split: s.
	^ ss select: [ :x | x isNotEmpty ] thenCollect: [ :x | Integer readFrom: x ]
]

{ #category : 'running' }
Runnable >> run: filenameOrInput [
	| d |
	d := self initFrom: filenameOrInput.
	^ 'Part1: ', (d part1 asString), ' Part2: ', (d part2 asString)
]
