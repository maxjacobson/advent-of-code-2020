memory = {}

MASK_PATTERN = /^mask = (.+)$/
MEM_PATTERN = /^mem\[(\d+)\] = (\d+)$/

def addresses(mask, unmasked_address)
  binary_unmasked_address = unmasked_address.to_s(2).rjust(36, '0')

  binary_masked_address = mask.split("").map.with_index { |char, index|
    if char == "X"
      'X'
    elsif char == '0'
      binary_unmasked_address[index]
    elsif char == '1'
      char
    else
      raise
    end
  }

  floating_indices = binary_masked_address.map.with_index { |char, index| index if char == "X" }.compact

  if floating_indices.any?
    max = floating_indices.length.times.map { '1' }.join('').to_i(2)

    (0..max).map { |n| n.to_s(2).rjust(floating_indices.length, "0") }.map do |overrides|
      binary_masked_address.tap do |address|
        floating_indices.zip(overrides.split("")).each do |index, override|
          address[index] = override
        end
      end.join
    end
  else
    [binary_masked_address]
  end
end

current_mask = nil
File.read("./input.txt").lines.each do |line|
  if (match = line.match(MASK_PATTERN))
    current_mask = match[1]
  elsif (match = line.match(MEM_PATTERN))
    unmasked_address = match[1].to_i
    value = match[2].to_i

    addresses(current_mask, unmasked_address).each do |address|
      memory[address] = value
    end
  else
    raise
  end
end

puts memory.values.sum # => 3296185383161
