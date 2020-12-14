schedule = File.read("./input.txt").lines[1]
Bus = Struct.new(:route_length, :offset)
schedule = schedule.
  chomp.
  split(",").
  map { |thing| thing == "x" ? nil : thing.to_i }.
  map.
  with_index { |id, index| Bus.new(id, index) if id }.
  compact

timestamp = 0
increment_by = 1

loop do
  satisfactory = schedule.take_while { |bus| (timestamp + bus.offset) % bus.route_length == 0 }

  if satisfactory.length == schedule.length
    # done!
    break
  elsif satisfactory.length > 1
    # We've found a case where the first 2 (or more) buses are lined up correctly.
    # They will continue to line up like this in a predictable fashion, every N
    # minutes, where N is the product of all of their route lengths. Armed with that
    # insight, we can skip ahead N minutes and check then, to see if the next bus has
    # snapped into place yet, or not. This helps us scan ahead at a fast pace.
    #
    # H/T @eutopian for sharing this insight
    increment_by = satisfactory.map(&:route_length).inject(:*)
  end

  timestamp += increment_by
end

puts timestamp
