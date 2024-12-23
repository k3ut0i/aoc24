instance creation
initFrom: fileNameOrInput
	| ls |
	ls := self getLines: fileNameOrInput.
	ylen := ls size.
	xlen := (ls at: 1) size.
	maze := Array withAll: ls.	