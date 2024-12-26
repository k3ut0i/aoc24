"
String manipulation behaviours
"
Trait {
	#name : 'StringManip',
	#category : 'AOC-24-Common',
	#package : 'AOC-24',
	#tag : 'Common'
}

{ #category : 'as yet unclassified' }
StringManip >> stringFromChars: charArray [

	^ (ByteArray newFromArray: (charArray collect: [ :x | x codePoint ])) asString
]
