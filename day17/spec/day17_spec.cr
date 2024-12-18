require "spec"
require "../src/day17"

describe Day17Solver do
  describe "#parse_registers" do
    it "returns a hash of registers" do
      registers = [
        "Register A: 729",
        "Register B: 2",
        "Register C: 3",
      ]
      Day17Solver.new.parse_registers(registers).should eq({a: 729, b: 2, c: 3}.to_h)
    end
  end

  describe "(when debugging)" do
    it "sets register B to 1 if register C contains 9, and the program is 2,6" do
      solver = Day17Solver.new({c: 9_i64}.to_h, [2_i8, 6_i8])
      solver.run
      solver.@registers[:b].should eq(1)
    end

    it "outputs 0,1,2 if register A contains 10, and the program is 5,0,5,1,5,4" do
      solver = Day17Solver.new({a: 10_i64}.to_h, [5_i8, 0_i8, 5_i8, 1_i8, 5_i8, 4_i8])
      solver.run
      solver.@output.should eq([0, 1, 2])
    end

    it "outputs 4,2,5,6,7,7,7,7,3,1,0 if register A contains 2024, and the program is 0,1,5,4,3,0" do
      solver = Day17Solver.new({a: 2024_i64}.to_h, [0_i8, 1_i8, 5_i8, 4_i8, 3_i8, 0_i8])
      solver.run
      solver.@output.should eq([4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0])
      solver.@registers[:a].should eq(0)
    end

    it "sets register B to 26 if register B contains 29, and the program is 1,7" do
      solver = Day17Solver.new({b: 29_i64}.to_h, [1_i8, 7_i8])
      solver.run
      solver.@registers[:b].should eq(26)
    end

    it "sets register B to 44354 if register B contains 2024 and register C contains 43690, and the program is 4,0" do
      solver = Day17Solver.new({b: 2024_i64, c: 43690_i64}.to_h, [4_i8, 0_i8])
      solver.run
      solver.@registers[:b].should eq(44354)
    end
  end

  describe "runs" do
    it "the first example and outputs 4,6,3,5,6,3,5,2,1,0" do
      solver = Day17Solver.new "data/task.example"
      solver.run
      solver.@output.should eq([4, 6, 3, 5, 6, 3, 5, 2, 1, 0])
    end

    it "the second example and outputs 0,3,5,4,3,0" do
      solver = Day17Solver.new "data/task2.example"
      result = solver.solve_second
      result.should eq(117440)
      solver.@output.should eq(solver.@program)
      solver.@output.should eq([0, 3, 5, 4, 3, 0])
    end

    it "some test 1" do
      solver = Day17Solver.new({a: 0_i64, b: 0_i64, c:0_i64}.to_h, [0_i8,3_i8,5_i8,4_i8,3_i8,0_i8])
      solver.run
      solver.@output.should eq([0])
    end
    it "some test 2" do
      solver = Day17Solver.new({a: 3_i64 << 6, b: 0_i64, c:0_i64}.to_h, [0_i8,3_i8,5_i8,4_i8,3_i8,0_i8])
      solver.run
      solver.@output.should eq([0, 3, 0])
    end
    it "some test 3" do
      solver = Day17Solver.new({a: 0b101011000000.to_i64, b: 0_i64, c:0_i64}.to_h, [0_i8,3_i8,5_i8,4_i8,3_i8,0_i8])
      solver.run
      solver.@output.should eq([0, 3, 5, 0])
    end
  end
end
