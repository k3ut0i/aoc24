helpers
getMul: aString
	| rx |
	rx := 'mul\(\d+,\d+\)|do|don''t'.
	^ rx asRegex matchesIn: aString.