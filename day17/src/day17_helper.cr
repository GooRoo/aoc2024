module Day17Helper
  @@ops_descriptions : Hash(Int8, Proc(Day17Solver, Int64, String)) = {
    0_i8 => ->(s : Day17Solver, v : Int64) { s.adv_str(v) },
    1_i8 => ->(s : Day17Solver, v : Int64) { s.bxl_str(v) },
    2_i8 => ->(s : Day17Solver, v : Int64) { s.bst_str(v) },
    3_i8 => ->(s : Day17Solver, v : Int64) { s.jnz_str(v) },
    4_i8 => ->(s : Day17Solver, v : Int64) { s.bxc_str(v) },
    5_i8 => ->(s : Day17Solver, v : Int64) { s.out_str(v) },
    6_i8 => ->(s : Day17Solver, v : Int64) { s.bdv_str(v) },
    7_i8 => ->(s : Day17Solver, v : Int64) { s.cdv_str(v) },
  }

  def combo_str(operand : Int64) : String
    case operand
    when 0, 1, 2, 3
      operand.to_s
    when 4
      "a"
    when 5
      "b"
    when 6
      "c"
    else
      "ERROR"
    end
  end

  def adv_str(operand : Int64)
    "a <== a >> #{combo_str(operand)}"
  end

  def bxl_str(operand : Int64)
    "b <== b ⊻ #{operand}"
  end

  def bst_str(operand : Int64)
    "b <== #{combo_str(operand)} ∧ 111"
  end

  def jnz_str(operand : Int64)
    "      jump #{operand} if a != 0"
  end

  def bxc_str(operand : Int64)
    "b <== b ⊻ c"
  end

  def out_str(operand : Int64)
    "      print #{combo_str(operand)} ∧ 111"
  end

  def bdv_str(operand : Int64)
    "b <== a >> #{combo_str(operand)}"
  end

  def cdv_str(operand : Int64)
    "b <== a >> #{combo_str(operand)}"
  end

  def inspect : String
    @program.each_slice(2).map do |pair|
      opcode, op = pair
      @@ops_descriptions[opcode].call(self, op.to_i64)
    end.join("\n")
  end
end
