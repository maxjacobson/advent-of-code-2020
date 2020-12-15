memory = {}

MASK_PATTERN = /^mask = (.+)$/
MEM_PATTERN = /^mem\[(\d+)\] = (\d+)$/

def masked(mask, value)
  binary_value = value.to_s(2).rjust(36, '0')
  masked_binary_value = mask.split("").map.with_index { |char, index|
    if char == "X"
      binary_value[index]
    else
      char
    end
  }.join
  masked_binary_value.to_i(2)
end

current_mask = nil
File.read("./input.txt").lines.each do |line|
  if (match = line.match(MASK_PATTERN))
    current_mask = match[1]
  elsif (match = line.match(MEM_PATTERN))
    address = match[1].to_i
    value = match[2].to_i
    memory[address] = masked(current_mask, value)
  else
    raise
  end
end

memory.values.sum # => 14862056079561
