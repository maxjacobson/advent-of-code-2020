Seat = Struct.new(:row, :column)

(((0..127).map { |row| (0..7).map { |col| Seat.new(row, col) } }).flatten -
File.read("./input.txt").lines.map { |line|
  directions = line.chomp.split("")
  row_directions = directions.slice(0, 7)
  column_directions = directions.slice(7, 3)

  row = row_directions.reduce((0..127).to_a) do |set, direction|
    half_size = set.length / 2
    if direction == "F"
      set.slice(0, half_size)
    elsif direction == "B"
      set.slice(half_size, set.length)
    else
      raise
    end
  end.first

  column = column_directions.reduce((0..7).to_a) do |set, direction|
    half_size = set.length / 2
    if direction == "L"
      set.slice(0, half_size)
    elsif direction == "R"
      set.slice(half_size, set.length)
    else
      raise
    end
  end.first

  Seat.new(row, column)
}).each do |seat|
  p seat
end
