as yet unclassified
stringFromChars: charArray

	^ (ByteArray newFromArray: (charArray collect: [ :x | x codePoint ])) asString