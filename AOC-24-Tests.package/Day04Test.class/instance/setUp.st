running
setUp
	super setUp.

	"Put here a common initialization logic for tests"
	directory := '/home/keutoi/projects/aoc24/resources/'.
	dayinstance := Day04 new initFrom: (directory, 'day04test').