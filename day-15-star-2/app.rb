nums = [1,0,16,5,17,4]
last_num = nums.last

tracker = nums.each_with_object({}).with_index do |(num, obj), index|
  obj[num] = [index + 1]
end

n = nums.length + 1

loop do
  if tracker[last_num].length == 1
    last_num = 0
  else
    last_num = tracker[last_num][-1] - tracker[last_num][-2]
  end
  tracker[last_num] ||= []
  tracker[last_num].push(n)

  break if n == 30000000
  n += 1
end

puts last_num
