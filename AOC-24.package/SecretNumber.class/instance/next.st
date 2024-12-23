arithmetic
next
	| s1 s2 s3 |
	s1 := (self mix: self * 64) prune.
	s2 := (self mix: s1 // 32) prune.
	s3 := (self mix: s2 * 2048) prune.
	^ s3.