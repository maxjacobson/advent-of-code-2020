fn find_pair(input: &str, sum: u32) -> Option<(u32, u32)> {
    let numbers: Vec<u32> = input
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

fn main() {
    let input: String = include_str!("../input.txt").to_string();

    let numbers: (u32, u32) = find_pair(&input, 2020).expect("Did not find a pair");

    println!("{}", numbers.0 * numbers.1);
}
