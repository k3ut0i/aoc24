accessing
at: i between: l and: u
	^ (diffData at: i) allSatisfy: [ :each | each between: l and: u ]