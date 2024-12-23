math
createDiffData | a b prev | 1 to: data size do: [ :i |
	a := data at: i.
	prev := a at: 1.
	b := a
		     collect: [ :x |
			     | tmp |
			     tmp := prev.
			     prev := x.
			     x - tmp ]
		     from: 2
		     to: a size.
	diffData addLast: b]