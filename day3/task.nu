use std log

export def solve-one [fileName: string] [nothing -> int] {
	log info $"Reading from ($fileName)"

	let result = (open $fileName
		| parse --regex '(mul\((?P<a>\d{1,3}),(?P<b>\d{1,3})\))'
		| into int a b
		| upsert c {|it| $it.a * $it.b}
		| math sum
		| get c
	)

	$result
}

export def solve-two [fileName: string] [nothing -> int] {
	log info $"Reading from ($fileName)"

	let result = (open $fileName
		| parse --regex r#'(?P<op>don't\(\)|do\(\)|mul\((?P<a>\d{1,3}),(?P<b>\d{1,3})\))'#
		| reduce --fold {do: true result: 0} {|it, acc|
			if $it.op == 'do()' {
				{do: true result: $acc.result}
			} else if $it.op == "don\'t()" {
				{do: false result: $acc.result}
			} else {
				if $acc.do {
					let v = $it | into int a b
					{do: $acc.do result: ($acc.result + $v.a * $v.b)}
				} else {
					$acc
				}
			}
		  }
		| get result
	)

	$result
}

export def test-all [] {
	log-result "firstExample" (solve-one "data/task1.example") 161
	log-result "firstTask" (solve-one "data/task1.data") 187833789
	log-result "secondExample" (solve-two "data/task2.example") 48
	log-result "secondTask" (solve-two "data/task2.data") 94455185
}

def log-result [
	name: string
	result: int
	control: int
] {
	let testLog = if $result == $control { ({|m| log info $m}) } else { ({|m| log error $m}) }

	do $testLog $"($name): expected ($control), got ($result)"
}
