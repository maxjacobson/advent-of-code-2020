data = File.read("./input.txt")

card_key, door_key = data.split("\n")
card_key = card_key.to_i # => 1614360
door_key = door_key.to_i # => 7734663

def loop_size_from(public_key)
  value = 1
  subject = 7
  loop_size = 0

  loop do
    loop_size += 1
    value = value * subject
    value = value % 20201227
    break if value == public_key
  end

  loop_size
end

def encryption_key_from(subject, loop_size)
  counter = 0
  value = 1

  loop do
    counter += 1
    value = value * subject
    value = value % 20201227
    break if counter == loop_size
  end

  value
end

card_loop_size = loop_size_from(card_key) # => 1182212
door_loop_size = loop_size_from(door_key) # => 4744857

encryption_key_from(card_key, door_loop_size) # => 5414549
encryption_key_from(door_key, card_loop_size) # => 5414549
