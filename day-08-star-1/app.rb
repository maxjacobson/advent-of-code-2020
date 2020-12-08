Instruction = Struct.new(:kind, :number)

def parse(instruction)
  kind, number = instruction.split(" ")
  Instruction.new(kind, number.to_i)
end

instructions = File.read("./input.txt").
  lines.
  map(&:chomp).
  map { |line| parse(line) }

acc = 0
indexes_visited = []
current_index = 0

loop do
  break if indexes_visited.include?(current_index)

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

puts acc
