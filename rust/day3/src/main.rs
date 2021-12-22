use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::vec::Vec;

fn open_file(path: &Path) -> std::fs::File {
    match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", path.display(), why),
        Ok(file) => file,
    }
}

fn open_file_as_str(path: &Path) -> String {
    let mut file_contents = String::new();
    match open_file(path).read_to_string(&mut file_contents) {
        Err(why) => panic!("couldn't read {}: {}", path.display(), why),
        Ok(_) => file_contents.trim_end_matches('\n').to_string(),
    }
}

fn d3calc_ratings(nums: &Vec<&str>, rel_01_abundance: &Vec<i32>, is_o2: bool) -> i32 {
    let mut nums = nums.clone();
    let mut rel_01_abundance = rel_01_abundance.clone();

    for i in 0..nums[0].len() {
        let keep;
        if is_o2 {
            keep = if rel_01_abundance[i] >= 0 { '1' } else { '0' };
        } else {
            keep = if rel_01_abundance[i] >= 0 { '0' } else { '1' };
        }

        for j in (0..nums.len()).rev() {
            if nums[j].as_bytes()[i] != keep as u8 {
                for (k, bit) in nums[j].chars().enumerate() {
                    match bit {
                        '0' => rel_01_abundance[k] += 1,
                        '1' => rel_01_abundance[k] -= 1,
                        _ => {}
                    }
                }
                nums.remove(j);
            }
        }
        if nums.len() <= 1 {
            break;
        }
    }

    let mut result = 0;
    for (i, bit) in nums[0].chars().rev().enumerate() {
        if bit == '1' {
            result += i32::pow(2, i as u32);
        }
    }

    result
}

fn day3() {
    let input = open_file_as_str(Path::new("../../day3_input.txt"));
    let parsed_input: Vec<&str> = input.split('\n').collect();

    let nr_positions = parsed_input[0].len();

    let mut rel_01_abundance = vec![0; nr_positions];
    for line in &parsed_input {
        for (i, char) in line.chars().enumerate() {
            match char {
                '0' => rel_01_abundance[i] -= 1,
                '1' => rel_01_abundance[i] += 1,
                _ => panic!("Unexpected character in input: {}", line),
            }
        }
    }

    // println!("{:?}", rel_01_abundance);

    let mut gamma_rate = 0;
    let mut epsilon_rate = 0;
    for (i, abundance) in rel_01_abundance.iter().rev().enumerate() {
        if *abundance > 0 {
            gamma_rate += i32::pow(2, i as u32);
        } else {
            epsilon_rate += i32::pow(2, i as u32);
        }
    }

    println!(
        "gamma_rate: {} epsilon_rate: {}, product: {}",
        gamma_rate,
        epsilon_rate,
        gamma_rate * epsilon_rate
    );

    let o2_rating = d3calc_ratings(&parsed_input, &rel_01_abundance, true);
    let co2_rating = d3calc_ratings(&parsed_input, &rel_01_abundance, false);

    println!(
        "o2_rating: {} co2_rating: {}, product: {}",
        o2_rating,
        co2_rating,
        o2_rating * co2_rating
    );
}

fn main() {
    day3();
}
