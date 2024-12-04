import XCTest
@testable import task

final class day4Test1: XCTestCase {
	let example = """
		MMMSXXMASM
		MSAMXMSMSA
		AMXSXMAAMM
		MSAMASMSMX
		XMASAMXAMM
		XXAMMXXAMA
		SMSMSASXSS
		SAXAMASAAA
		MAMMMXMMMM
		MXMXAXMASX
	"""

	func testStaticExample() throws {
		XCTAssertEqual(solveFirst(example), 18)
	}

    func testExample() throws {
		XCTAssertEqual(getResult(fromFile: "data/task1.example", solveFirst), 18)
    }

	func testData() throws {
		XCTAssertEqual(getResult(fromFile: "data/task1.data", solveFirst), 2557)
	}
}

final class day4Test2: XCTestCase {
	let example = """
		.M.S......
		..A..MSMS.
		.M.S.MAA..
		..A.ASMSM.
		.M.S.M....
		..........
		S.S.S.S.S.
		.A.A.A.A..
		M.M.M.M.M.
		..........
		"""

	func testStaticExample() throws {
		XCTAssertEqual(solveSecond(example), 9)
	}

	func testExample() throws {
		XCTAssertEqual(getResult(fromFile: "data/task2.example", solveSecond), 9)
	}

	func testData() throws {
		XCTAssertEqual(getResult(fromFile: "data/task2.data", solveSecond), 1854)
	}
}
