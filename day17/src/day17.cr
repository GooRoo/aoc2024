require "./day17_helper.cr"

class NoSolution < Exception; end

class Halted < Exception; end

class InvalidOperand(T) < Exception
  @operand : T
  getter :operand

  def initialize(@operand); end
end

class Day17Solver
  include Day17Helper

  @ptr : Int16 = 0
  @registers = {} of Symbol => Int64
  @program = [] of Int8

  @output = [] of Int8

  @@ops : Hash(Int8, Proc(Day17Solver, Int64, Int16)) = {
    0_i8 => ->(s : Day17Solver, v : Int64) { s.adv!(v) },
    1_i8 => ->(s : Day17Solver, v : Int64) { s.bxl!(v) },
    2_i8 => ->(s : Day17Solver, v : Int64) { s.bst!(v) },
    3_i8 => ->(s : Day17Solver, v : Int64) { s.jnz!(v) },
    4_i8 => ->(s : Day17Solver, v : Int64) { s.bxc!(v) },
    5_i8 => ->(s : Day17Solver, v : Int64) { s.out!(v) },
    6_i8 => ->(s : Day17Solver, v : Int64) { s.bdv!(v) },
    7_i8 => ->(s : Day17Solver, v : Int64) { s.cdv!(v) },
  }

  def initialize; end

  def initialize(file_path : String)
    @registers, @program = read_file Path.new(file_path)
  end

  def initialize(@registers, @program); end

  def read_file(file_path : Path) : Tuple(Hash(Symbol, Int64), Array(Int8))
    lines = File.read(file_path).lines
    registers = lines[0..2]
    instructions = lines[4]
    {parse_registers(registers), parse_program(instructions)}
  end

  def parse_registers(registers : Array(String)) : Hash(Symbol, Int64)
    result = {} of Symbol => Int64
    registers.each do |register|
      if match = register.match(/Register ([ABC]):\s+(\d+)/)
        case match[1]
        when "A"
          result[:a] = match[2].to_i64
        when "B"
          result[:b] = match[2].to_i64
        when "C"
          result[:c] = match[2].to_i64
        end
      end
    end
    result
  end

  def parse_program(instructions : String) : Array(Int8)
    program = "Program: "
    if instructions.starts_with?(program)
      instructions[program.size..-1].split(",").map do |instruction|
        instruction.to_i8
      end
    else
      [] of Int8
    end
  end

  def combo(operand : Int64) : Int64
    # could be simply
    #   operand < 4 ? operand : @registers[operand - 4]
    # but I chose to use :a, :b, and :c 🫤
    case operand
    when 0, 1, 2, 3
      operand
    when 4
      @registers[:a]
    when 5
      @registers[:b]
    when 6
      @registers[:c]
    else
      raise InvalidOperand.new operand
    end
  end

  def lit(operand : Int64) : Int64
    operand
  end

  def adv!(operand : Int64)
    @registers[:a] >>= combo(operand)
    @ptr += 2
  end

  def bxl!(operand : Int64)
    @registers[:b] ^= lit(operand)
    @ptr += 2
  end

  def bst!(operand : Int64)
    @registers[:b] = combo(operand) & 0b111
    @ptr += 2
  end

  def jnz!(operand : Int64)
    if @registers[:a] == 0
      @ptr += 2
    else
      @ptr = lit(operand).to_i16
    end
  end

  def bxc!(operand : Int64)
    @registers[:b] ^= @registers[:c]
    @ptr += 2
  end

  def out!(operand : Int64)
    @output << (combo(operand) & 0b111).to_i8!
    @ptr += 2
  end

  def bdv!(operand : Int64)
    @registers[:b] = @registers[:a] >> combo(operand)
    @ptr += 2
  end

  def cdv!(operand : Int64)
    @registers[:c] = @registers[:a] >> combo(operand)
    @ptr += 2
  end

  def run
    run { }
  end

  def run(&)
    @ptr = 0

    until @ptr >= @program.size
      opcode = @program[@ptr]
      if @ptr + 1 < @program.size
        operand = @program[@ptr + 1]
      else
        raise Halted.new
      end
      @@ops[opcode].call(self, operand.to_i64)
      if opcode == 5_i8
        yield @output.last
      end
    end
    true
  end

  def solve_first
    begin
      run
    rescue ex : Halted
      puts "Halted with output: #{@output}"
    rescue ex : InvalidOperand
      puts "Invalid operand: #{ex.operand}"
    end
    @output.join(",")
  end

  def solve_second(*, start = -1)
    orig_registers = @registers.clone
    target = @program.dup

    new_a = start
    until @output == target
      new_a += 1
      if (new_a % 100000) == 0
        puts "Trying #{new_a}"
      end
      @registers = orig_registers.clone
      @registers[:a] = new_a
      @output.clear

      run do |output|
        break if @output.size > target.size ||
                 (@output.size > 0 &&
                 @output.last != @program[@output.size - 1])
      end
    end
    new_a
  end

  def solve_better_second
    orig_registers = @registers

    result = 0b101_i64
    target = @program.reverse
    instruction = 0

    last_Is = [] of Int32

    until instruction == target.size
      puts "===================="
      puts "iteration: #{instruction}"
      puts "Base: #{result}"

      i = -1
      while i < 8
        i += 1
        puts "------------------"
        puts "Trying #{result | i}"

        @registers = orig_registers
        @registers[:a] = result | i
        @output.clear

        begin
          if run { |output|
               puts "Output: #{output}"
               puts "Target: #{target[instruction]}"
               if output == target[instruction]
                break false
               else
                break true
               end
             }

            if i >= 7
              raise NoSolution.new
            end
          else
            puts "FOUND IT"
            if instruction + 1 == target.size
              return result | i
            end

            last_Is << i
            result |= i
            result <<= 3
            break
          end
        rescue ex : Halted
          puts "Halted"
          next
        rescue ex : NoSolution
          i = last_Is.pop
          result >>= 3
          instruction -= 1
        end
      end

      instruction += 1
    end

    result
  end
end
