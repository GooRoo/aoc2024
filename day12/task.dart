import 'dart:io';

const FILE_NAME = 'data/task.data';

class Point {
  final int row;
  final int col;
  const Point(this.row, this.col);

  Point above() => Point(row - 1, col);
  Point below() => Point(row + 1, col);
  Point left() => Point(row, col - 1);
  Point right() => Point(row, col + 1);

  Point aboveLeft() => Point(row - 1, col - 1);
  Point aboveRight() => Point(row - 1, col + 1);
  Point belowLeft() => Point(row + 1, col - 1);
  Point belowRight() => Point(row + 1, col + 1);

  Point operator +(Point rhs) => Point(row + rhs.row, col + rhs.col);

  @override
  String toString() {
    return '($row, $col)';
  }

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    if (other is! Point) return false;
    return row == other.row && col == other.col;
  }

  @override
  int get hashCode => row.hashCode ^ col.hashCode;
}

enum Side { Above, Below, Left, Right }

class Edge {
  Point start;
  Point end;
  Side direction;
  Edge(this.start, this.end, this.direction);

  @override
  String toString() => '$start -> $end ($direction)';

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    if (other is! Edge) return false;
    return start == other.start &&
        end == other.end &&
        direction == other.direction;
  }

  @override
  int get hashCode => start.hashCode ^ end.hashCode ^ direction.hashCode;
}

class Matrix<T> {
  List<List<T>> data;
  final int rows;
  final int cols;

  Matrix(this.data)
      : rows = data.length,
        cols = data[0].length;

  T operator [](Point p) => data[p.row][p.col];

  void operator []=(Point p, T value) {
    data[p.row][p.col] = value;
  }

  bool isInside(Point p) {
    return p.row >= 0 && p.row < rows && p.col >= 0 && p.col < cols;
  }

  int perimeter(Point p) {
    int perim = 0;
    final cur = this[p];
    if (!isInside(p.above()) || cur != this[p.above()]) perim++;
    if (!isInside(p.below()) || cur != this[p.below()]) perim++;
    if (!isInside(p.left()) || cur != this[p.left()]) perim++;
    if (!isInside(p.right()) || cur != this[p.right()]) perim++;
    return perim;
  }

  @override
  String toString() {
    return data.map((row) => row.join('')).join('\n');
  }
}

class Region {
  int id;
  int area = 0;
  int perim = 0;

  Set<Point> points = {};
  Region(this.id);

  void addPoint(Point p, int perimeter) {
    points.add(p);
    area++;
    perim += perimeter;
  }

  int cost() {
    return area * perim;
  }

  int discount_cost() {
    return area * edges().length;
  }

  Set<Edge> edges() {
    Edge? findEdge(Point point, Side side) {
      const check = {
        Side.Above: Point(0, -1),
        Side.Below: Point(0, 1),
        Side.Left: Point(-1, 0),
        Side.Right: Point(1, 0),
      };
      const move = {
        Side.Above: Point(-1, 0),
        Side.Below: Point(1, 0),
        Side.Left: Point(0, 1),
        Side.Right: Point(0, -1),
      };

      if (!points.contains(point + check[side]!)) {
        var start = point;
        var end = point;
        while (points.contains(end) && !points.contains(end + check[side]!)) {
          end = end + move[side]!;
        }
        while (
            points.contains(start) && !points.contains(start + check[side]!)) {
          start = start + move[side]!;
        }
        return Edge(start + move[side]!, end + move[side]!, side);
      } else {
        return null;
      }
    }

    Set<Edge> result = {};
    for (var point in points) {

      for (var side in Side.values) {
        final edge = findEdge(point, side);
        if (edge != null) {
          result.add(edge);
        }
      }
    }

    return result;
  }

  @override
  String toString() {
    return 'Region $id: ${points.length} points: $points';
  }
}

Matrix<String> readFile(String fileName) {
  final lines = File(fileName).readAsLinesSync();
  return Matrix(lines.map((line) => line.split('')).toList());
}

List<Region> findRegions(Matrix matrix) {
  final rows = matrix.rows;
  final cols = matrix.cols;

  List<Region> regions = [];

  Matrix<bool> visited =
      Matrix(List.generate(rows, (_) => List.generate(cols, (_) => false)));

  int regionId = 0;

  void dfs(Point point, String char, int id) {
    if (!matrix.isInside(point) || matrix[point] != char || visited[point]) {
      return;
    }

    if (regions.length <= id) {
      regions.add(Region(id));
    }
    regions[id].addPoint(point, matrix.perimeter(point));
    visited[point] = true;

    dfs(point.above(), char, id);
    dfs(point.below(), char, id);
    dfs(point.left(), char, id);
    dfs(point.right(), char, id);
  }

  for (int r = 0; r < rows; r++) {
    for (int c = 0; c < cols; c++) {
      final p = Point(r, c);
      if (!visited[p]) {
        dfs(p, matrix[p], regionId);
        regionId++;
      }
    }
  }

  return regions;
}

void solveFirst() {
  final matrix = readFile(FILE_NAME);
  final regions = findRegions(matrix);
  print(regions.fold<int>(0, (acc, region) => acc + region.cost()));
}

void solveSecond() {
  final matrix = readFile(FILE_NAME);
  final regions = findRegions(matrix);
  print(regions.fold<int>(0, (acc, region) => acc + region.discount_cost()));
}

void main() {
  solveFirst();
  solveSecond();
}
