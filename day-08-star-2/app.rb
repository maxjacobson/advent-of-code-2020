require "irb"

Instruction = Struct.new(:kind, :number)
GoingIntoInfiniteLoop = Class.new(StandardError)

def parse(instruction)
  kind, number = instruction.split(" ")
  Instruction.new(kind, number.to_i)
end


def execute(instructions)
  acc = 0
  indexes_visited = []
  current_index = 0

  loop do
    if current_index == instructions.length
      # Exited correctly
      break
    end

    if indexes_visited.include?(current_index)
      raise GoingIntoInfiniteLoop
    end

    indexes_visited << current_index

    instruction = instructions.fetch(current_index)

    case instruction.kind
    when "acc"
      acc += instruction.number
      current_index += 1
    when "jmp"
      current_index += instruction.number
    when "nop"
      current_index += 1
    else
      raise "unknown kind: #{instruction.kind}"
    end
  end

  acc
end

last_modified = -1
loop do
  instructions = File.read("./input.txt").
    lines.
    map(&:chomp).
    map { |line| parse(line) }

  index = instructions.index.with_index do |instruction, i|
    i > last_modified && ["nop", "jmp"].include?(instruction.kind)
  end
  instructions[index].kind = instructions[index].kind == "jmp" ? "nop" : "jmp"
  last_modified = index

  begin
    acc = execute(instructions)
    puts acc
    break
  rescue GoingIntoInfiniteLoop
    # failed
  end
end

