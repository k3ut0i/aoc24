accessing
hasMasAt: aPoint
	| w |
	w := self diagAt: aPoint.
	^ ($A = (self at: aPoint)) ifTrue: [ PossiblePatterns anySatisfy: [ :p | p = w ] ] ifFalse: [ false ].