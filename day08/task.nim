import std/strformat
import std/strutils
import std/sets
import std/tables

const inputPath = "data/task.data"
const fileContent = staticRead(inputPath)
const lines = fileContent.splitLines()
const width = if lines.len > 0: lines[0].len else: 0
const height = lines.len - 1
stdout.writeLine(width.intToStr & "×" & height.intToStr)

type Matrix = array[height, array[width, char]]

const matrix = block:
  var result: Matrix
  for y in 0..<height:
    for x in 0..<width:
      result[y][x] = lines[y][x]
  result

const theTable = block:
    var table: Table[char, seq[(int, int)]] = initTable[char, seq[(int, int)]]()
    for y in 0..<height:
      for x in 0..<width:
        let key = matrix[y][x]
        if matrix[y][x] != '.':
          if not table.hasKey(key):
            table[key] = @[]
          table[key].add((y, x))
    table

for y in 0..<height:
  for x in 0..<width:
    stdout.write(matrix[y][x])
    stdout.write(' ')
  stdout.write('\n')

for frequency, antennas  in theTable:
  stdout.write(frequency)
  stdout.write(' ')
  for antenna in antennas:
    stdout.write(antenna)
    stdout.write(' ')
  stdout.write('\n')

proc `-`[T: int | int8 | int16 | int32 | int64](a, b: (T, T)): (T, T) =
  ((a[0] - b[0]), (a[1] - b[1]))

proc `+`[T: int | int8 | int16 | int32 | int64](a, b: (T, T)): (T, T) =
  ((a[0] + b[0]), (a[1] + b[1]))

proc toString(a: (int, int)): string =
  &"{a[0]},{a[1]}"

var antinodes = initHashSet[(int, int)]()

proc antinode(a, b: (int, int)): (bool, (int, int)) =
  let distance = (a - b)
  let anti = a + distance
  stdout.writeLine(&"{a} <-> {b}   ====> {anti}")

  if anti[0] in 0..<height and anti[1] in 0..<width:
    discard antinodes.containsOrIncl(anti)
    return (true, anti)
  else:
    return (false, (-1, -1))

proc solveFirst() =
  for frequency, antennas  in theTable:
    for i in 0..<antennas.len - 1:
      let current = antennas[i]
      for j in i + 1..<antennas.len:
        let next = antennas[j]
        discard antinode(current, next)
        discard antinode(next, current)

proc solveSecond() =
  for frequency, antennas  in theTable:
    for i in 0..<antennas.len - 1:
      let current = antennas[i]
      for j in i + 1..<antennas.len:
        discard antinodes.containsOrIncl(current)
        discard antinodes.containsOrIncl(antennas[j])
        for _, (a, b) in [(current, antennas[j]), (antennas[j], current)]:
          var first = a
          var second = b
          var (res, coord) = antinode(first, second)
          while res:
            second = first
            first = coord
            (res, coord) = antinode(first, second)

proc printMatrix() =
  var newMatrix = matrix
  for antinode in antinodes:
    newMatrix[antinode[0]][antinode[1]] = '#'

  stdout.write("    ")
  for x in 0..<width:
    stdout.write((x mod 10).intToStr)
    stdout.write(' ')
  stdout.write('\n')
  for y in 0..<height:
    stdout.write(&"{y mod 10} | ")
    for x in 0..<width:
      stdout.write(newMatrix[y][x])
      stdout.write(' ')
    stdout.write('\n')

  stdout.writeLine(&"Total antinodes: {antinodes.len}")


solveFirst()
printMatrix()

antinodes.clear

solveSecond()
printMatrix()
