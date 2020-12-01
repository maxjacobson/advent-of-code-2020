fn find_pair(input: &str, sum: i32) -> Option<(i32, i32)> {
    let numbers: Vec<i32> = input
        .lines()
        .map(|line| line.parse().expect("Could not parse line"))
        .collect();

    for number in &numbers {
        let other_number = sum - number;

        if numbers.contains(&other_number) {
            return Some((*number, other_number));
        }
    }

    None
}

fn find_triplet(input: &str, sum: i32) -> Option<(i32, i32, i32)> {
    let numbers: Vec<i32> = input
        .lines()
        .map(|line| line.parse().expect("Could not parse line"))
        .collect();

    for number in &numbers {
        let other_number = sum - number;

        if let Some(pair) = find_pair(input, other_number) {
            return Some((*number, pair.0, pair.1));
        }
    }

    None
}

fn main() {
    let input: String = include_str!("../input.txt").to_string();

    let numbers: (i32, i32, i32) = find_triplet(&input, 2020).expect("Did not find a triplet");

    println!("{}", numbers.0 * numbers.1 * numbers.2);
}
