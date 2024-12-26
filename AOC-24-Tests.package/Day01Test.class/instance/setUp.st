running
setUp
	| fileOrString |
	super setUp.

	"Put here a common initialization logic for tests"
	fileOrString := directory , 'day01'.
	dayinstance := Day01 new initFrom: fileOrString.