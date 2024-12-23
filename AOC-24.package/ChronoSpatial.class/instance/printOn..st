printing
printOn: aStream
	'A [', rega asString, '] ', 'B [', regb asString, '] ',	 'C [', regc asString, '] ' printOn: aStream.
	aStream lf.
	outqueue printOn: aStream.