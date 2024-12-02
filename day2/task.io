List mapPairs := method(
	left := call argAt(0) name
	right := call argAt(1) name
	body := call argAt(2)

	context := Object clone prependProto(call sender)

	if(call sender hasLocalSlot("self"),
		context setSlot("self", call sender self)
	)

	result := List clone

	for (i, 0, self size - 2,
		context setSlot(left, self at(i))
		context setSlot(right, self at(i + 1))
		result append(context doMessage(body))
	)

	result
)

checkSafe := method(report,
	increasing := true
	decreasing := true

	report mapPairs(l, r,
		increasing = increasing and l < r
		decreasing = decreasing and l > r

		# "#{l}, #{r}: #{(increasing or decreasing) and (l - r) abs <= 3}" interpolate println

		(increasing or decreasing) and (l - r) abs >= 1 and (l - r) abs <= 3
	)
)

dropAndRecheck := method(report, index,
	altReport := report clone
	altReport removeAt(index)
	result := checkSafe(altReport) reduce(and)
	scissors := "✂️" asUTF8
	"\tattempt: #{altReport map(asString) insertAt(scissors, index)} - #{result}" interpolate println
	result
)

readReports := method(fileName, withProblemDampener,
	theFile := File openForReading(fileName)
	reports := theFile readLines map(line, line split map(asNumber))
	theFile close

	reports reduce(acc, report,
		("---------------\n" .. report) print

		safe := checkSafe(report)

		if (safe reduce(and),
			" - all good" println
			acc + 1
		,
			if (withProblemDampener,
				firstFalse := safe indexOf(false)
				" - first false at #{firstFalse}" interpolate println

				stepBack := if(firstFalse > 0,
					dropAndRecheck(report, firstFalse - 1)
				,
					false
				)

				maybeNow1 := dropAndRecheck(report, firstFalse)

				maybeNow2 := dropAndRecheck(report, firstFalse + 1)

				"Overall result: #{(maybeNow1) or (maybeNow2)}" interpolate println

				if (stepBack or maybeNow1 or maybeNow2, acc + 1, acc)
			,
				acc
			)
		)
	, 0)
)

testAll := method(
	# ----- task 1 -----
	theExample1 := readReports("data/task.example", false)
	allValid1 := readReports("data/task.valid", false)
	fullData1 := readReports("data/task.data", false)

	# ----- task 2 -----
	theExample2 := readReports("data/task.example", true)
	allValid2 := readReports("data/task.valid", true)
	fullData2 := readReports("data/task.data", true)

	# ----- results -----
	"Without problem dampener: " println
	("\ttask.example: " .. theExample1 == 2) println
	("\ttask.valid: " .. allValid1 == 0) println
	("\ttask.data: " .. fullData1 == 591) println

	"With problem dampener: " println
	("\ttask.example: " .. theExample2 == 4) println
	("\ttask.valid: " .. allValid2 == 10) println
	("\ttask.data: " .. fullData2 == 621) println
)

isLaunchScript ifTrue(testAll)
