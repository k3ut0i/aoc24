helpers
extractNum: mulString
	| stripped nums |
	stripped := (mulString withoutPrefix: 'mul(') withoutSuffix: ')'.
	nums := (stripped splitOn: ',') collect: [:s | s asNumber ].
	self assert: [ nums size = 2 ].
	^ (nums at: 1) * (nums at: 2).