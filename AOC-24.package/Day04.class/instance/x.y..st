accessing - array
x: xc y: yc
	^ ({ (0 < xc). (0 < yc). (xc <= xlen). (yc <= ylen) } allSatisfy: [ :b | b ])
			ifTrue: [ (maze at: yc) at: xc ]
			ifFalse: [ $0 ]