import Foundation

func findXmas(in input: String) -> Int {
	let xmas: String = "XMAS"
	var count = 0

	if input.count < xmas.count {
		return 0
	}

	for i in 0...(input.count - xmas.count) {
		let start = input.index(input.startIndex, offsetBy: i)
		let end = input.index(start, offsetBy: xmas.count)
		let sub = String(input[start..<end])

		if sub == xmas { count += 1 }
		if sub == String(xmas.reversed()) { count += 1 }
	}

	return count
}

func solveFirst(_ input: String) -> Int {
	let matrix = input.split(separator: "\n").map { Array($0) }
	var count = 0

	for line in matrix {
		count += findXmas(in: String(line))
	}

	let transposed = (0..<matrix[0].count).map { col in
		matrix.map { $0[col] }
	}

	for line in transposed {
		count += findXmas(in: String(line))
	}

	let diagonals = (0..<(matrix.count + matrix[0].count - 1)).map { d in
		matrix.enumerated().compactMap { (i, row) in
			let j = d - (matrix.count - 1 - i)
			return (j >= 0 && j < row.count) ? row[j] : nil
		}
	}

	for line in diagonals {
		count += findXmas(in: String(line))
	}

	let antiDiagonals = (0..<(matrix.count + matrix[0].count - 1)).map { d in
		matrix.enumerated().compactMap { (i, row) in
			let j = d - i
			return (j >= 0 && j < row.count) ? row[j] : nil
		}
	}

	for line in antiDiagonals {
		count += findXmas(in: String(line))
	}

	return count
}

func solveSecond(_ input: String) -> Int {
	let matrix = input.split(separator: "\n").map { Array($0) }
	var count = 0

	func checkCorners(_ topLeft: Character, _ topRight: Character, _ bottomLeft: Character, _ bottomRight: Character) -> Bool {
		return (topLeft == "M" && topRight == "M" && bottomLeft == "S" && bottomRight == "S")
			|| (topLeft == "S" && topRight == "S" && bottomLeft == "M" && bottomRight == "M")
			|| (topLeft == "M" && topRight == "S" && bottomLeft == "M" && bottomRight == "S")
			|| (topLeft == "S" && topRight == "M" && bottomLeft == "S" && bottomRight == "M")
	}

	for i in 1..<matrix.count - 1 {
		for j in 1..<matrix[i].count - 1 {
			if matrix[i][j] != "A" { continue }

			if checkCorners(matrix[i-1][j-1], matrix[i-1][j+1], matrix[i+1][j-1], matrix[i+1][j+1]) {
				count += 1
			}
		}
	}

	return count
}

func getResult(fromFile: String, _ solver: ((String) -> Int)) -> Int {
	if let contents = try? String(contentsOfFile: fromFile, encoding: .utf8) {
		return solver(contents)
	} else {
		return -1
	}
}

print(getResult(fromFile: "data/task1.example", solveFirst))
print(getResult(fromFile: "data/task1.data", solveFirst))
print(getResult(fromFile: "data/task2.example", solveSecond))
print(getResult(fromFile: "data/task2.data", solveSecond))
