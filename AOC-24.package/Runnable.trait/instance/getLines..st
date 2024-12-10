io-string
getLines: filename
	| f lines |
	f := File named: filename asFileReference fullName.
	lines := f readStreamDo: [ :s | Character lf split: s upToEnd asString ].
	((lines last) isAllSeparators) ifTrue: [ lines removeLast ].
	^ lines