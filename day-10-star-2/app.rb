def paths_from(voltages, from, to)
  return $memo[from] if $memo[from]
  children = voltages.select { |voltage| voltage > from && voltage <= from + 3 }

  children.reduce(0) do |acc, child|
    n = $memo[child] =
      if child == to
        1
      else
        paths_from(voltages, child, to)
      end

    acc + n
  end
end

$memo = {}
adapter_voltages = File.read("./input.txt").lines.map(&:to_i).sort
to = adapter_voltages.last + 3
adapter_voltages << to
puts paths_from(adapter_voltages, 0, to)
