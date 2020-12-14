# H/T Ashley/Archana for pairing on this
starting_time, schedule = File.read("./input.txt").lines

starting_time = starting_time.chomp.to_i
schedule = schedule.chomp.split(",").map { |thing| thing.to_i unless thing == "x" }.compact

def buses_at(time, schedule)
  schedule.select { |bus| time % bus == 0 }
end

i = 0
loop do
  time = starting_time + i
  buses = buses_at(time, schedule)
  if buses.length > 1
    raise "wtf"
  elsif buses.length == 1
    bus = buses.first
    waiting_time = time - starting_time
    raise (waiting_time * bus).to_s
  else
    i += 1
  end
end
