use std::collections::HashSet;
use std::fs::File;
use std::io::{self, prelude::*};
use std::ops::*;
use std::path::Path;
use std::vec::Vec;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Axis {
    X,
    Y,
    Z,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Sign {
    Pos,
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SignedAxis {
    sign: Sign,
    axis: Axis,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Orientation {
    direction: SignedAxis,
    rotation: SignedAxis,
}

#[rustfmt::skip]
static ALL_POSSIBLE_ORIENTATIONS: [Orientation; 24] = [
    Orientation{direction: SignedAxis{axis: Axis::X, sign: Sign::Pos}, rotation: SignedAxis{axis: Axis::Y, sign: Sign::Pos}},
    Orientation{direction: SignedAxis{axis: Axis::X, sign: Sign::Pos}, rotation: SignedAxis{axis: Axis::Z, sign: Sign::Pos}},
    Orientation{direction: SignedAxis{axis: Axis::X, sign: Sign::Pos}, rotation: SignedAxis{axis: Axis::Y, sign: Sign::Neg}},
    Orientation{direction: SignedAxis{axis: Axis::X, sign: Sign::Pos}, rotation: SignedAxis{axis: Axis::Z, sign: Sign::Neg}},

    Orientation{direction: SignedAxis{axis: Axis::Y, sign: Sign::Pos}, rotation: SignedAxis{axis: Axis::X, sign: Sign::Neg}},
    Orientation{direction: SignedAxis{axis: Axis::Y, sign: Sign::Pos}, rotation: SignedAxis{axis: Axis::Z, sign: Sign::Pos}},
    Orientation{direction: SignedAxis{axis: Axis::Y, sign: Sign::Pos}, rotation: SignedAxis{axis: Axis::X, sign: Sign::Pos}},
    Orientation{direction: SignedAxis{axis: Axis::Y, sign: Sign::Pos}, rotation: SignedAxis{axis: Axis::Z, sign: Sign::Neg}},

    Orientation{direction: SignedAxis{axis: Axis::Z, sign: Sign::Pos}, rotation: SignedAxis{axis: Axis::Y, sign: Sign::Pos}},
    Orientation{direction: SignedAxis{axis: Axis::Z, sign: Sign::Pos}, rotation: SignedAxis{axis: Axis::X, sign: Sign::Neg}},
    Orientation{direction: SignedAxis{axis: Axis::Z, sign: Sign::Pos}, rotation: SignedAxis{axis: Axis::Y, sign: Sign::Neg}},
    Orientation{direction: SignedAxis{axis: Axis::Z, sign: Sign::Pos}, rotation: SignedAxis{axis: Axis::X, sign: Sign::Pos}},


    Orientation{direction: SignedAxis{axis: Axis::X, sign: Sign::Neg}, rotation: SignedAxis{axis: Axis::Y, sign: Sign::Neg}},
    Orientation{direction: SignedAxis{axis: Axis::X, sign: Sign::Neg}, rotation: SignedAxis{axis: Axis::Z, sign: Sign::Pos}},
    Orientation{direction: SignedAxis{axis: Axis::X, sign: Sign::Neg}, rotation: SignedAxis{axis: Axis::Y, sign: Sign::Pos}},
    Orientation{direction: SignedAxis{axis: Axis::X, sign: Sign::Neg}, rotation: SignedAxis{axis: Axis::Z, sign: Sign::Neg}},

    Orientation{direction: SignedAxis{axis: Axis::Y, sign: Sign::Neg}, rotation: SignedAxis{axis: Axis::X, sign: Sign::Pos}},
    Orientation{direction: SignedAxis{axis: Axis::Y, sign: Sign::Neg}, rotation: SignedAxis{axis: Axis::Z, sign: Sign::Pos}},
    Orientation{direction: SignedAxis{axis: Axis::Y, sign: Sign::Neg}, rotation: SignedAxis{axis: Axis::X, sign: Sign::Neg}},
    Orientation{direction: SignedAxis{axis: Axis::Y, sign: Sign::Neg}, rotation: SignedAxis{axis: Axis::Z, sign: Sign::Neg}},

    Orientation{direction: SignedAxis{axis: Axis::Z, sign: Sign::Neg}, rotation: SignedAxis{axis: Axis::Y, sign: Sign::Pos}},
    Orientation{direction: SignedAxis{axis: Axis::Z, sign: Sign::Neg}, rotation: SignedAxis{axis: Axis::X, sign: Sign::Pos}},
    Orientation{direction: SignedAxis{axis: Axis::Z, sign: Sign::Neg}, rotation: SignedAxis{axis: Axis::Y, sign: Sign::Neg}},
    Orientation{direction: SignedAxis{axis: Axis::Z, sign: Sign::Neg}, rotation: SignedAxis{axis: Axis::X, sign: Sign::Neg}},
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point3 {
    x: i32,
    y: i32,
    z: i32,
}

impl Add<Point3> for Point3 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Point3 {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl Add<i32> for Point3 {
    type Output = Self;

    fn add(self, other: i32) -> Self {
        Point3 {
            x: self.x + other,
            y: self.y + other,
            z: self.z + other,
        }
    }
}

impl Sub<Point3> for Point3 {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Point3 {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
}

impl Sub<i32> for Point3 {
    type Output = Self;

    fn sub(self, other: i32) -> Self {
        Point3 {
            x: self.x - other,
            y: self.y - other,
            z: self.z - other,
        }
    }
}

impl Mul<Point3> for Point3 {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Point3 {
            x: self.x * other.x,
            y: self.y * other.y,
            z: self.z * other.z,
        }
    }
}

impl Mul<i32> for Point3 {
    type Output = Self;

    fn mul(self, other: i32) -> Self {
        Point3 {
            x: self.x * other,
            y: self.y * other,
            z: self.z * other,
        }
    }
}

impl Div<Point3> for Point3 {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        Point3 {
            x: self.x / other.x,
            y: self.y / other.y,
            z: self.z / other.z,
        }
    }
}

impl Div<i32> for Point3 {
    type Output = Self;

    fn div(self, other: i32) -> Self {
        Point3 {
            x: self.x / other,
            y: self.y / other,
            z: self.z / other,
        }
    }
}

impl Neg for Point3 {
    type Output = Self;

    fn neg(self) -> Self {
        Point3 {
            x: -self.x,
            y: -self.y,
            z: -self.z,
        }
    }
}

impl Point3 {
    fn new(scalar: i32) -> Self {
        Point3 {
            x: scalar,
            y: scalar,
            z: scalar,
        }
    }

    fn abs(self) -> Self {
        return Self {
            x: self.x.abs(),
            y: self.y.abs(),
            z: self.z.abs(),
        };
    }

    fn manhattan_distance(self, other: Point3) -> i32 {
        let vals = (self - other).abs();
        vals.x + vals.y + vals.z
    }

    fn from_iter<T: Iterator<Item = i32>>(mut data: T) -> Self {
        Point3 {
            x: data.next().unwrap(),
            y: data.next().unwrap(),
            z: data.next().unwrap(),
        }
    }

    /// Rotates a point around an axis in place according to the right hand rule.
    fn rotate_over(&mut self, axis: SignedAxis) -> &mut Self {
        match axis.axis {
            Axis::X => {
                let tmp_y = self.y;
                self.y = self.z;
                self.z = tmp_y;

                match axis.sign {
                    Sign::Pos => self.y = -self.y,
                    Sign::Neg => self.z = -self.z,
                }
            }
            Axis::Y => {
                let tmp_x = self.x;
                self.x = self.z;
                self.z = tmp_x;

                match axis.sign {
                    Sign::Pos => self.z = -self.z,
                    Sign::Neg => self.x = -self.x,
                }
            }
            Axis::Z => {
                let tmp_x = self.x;
                self.x = self.y;
                self.y = tmp_x;

                match axis.sign {
                    Sign::Pos => self.x = -self.x,
                    Sign::Neg => self.y = -self.y,
                }
            }
        }
        self
    }
}

impl SignedAxis {
    fn as_vec3(&self) -> Point3 {
        let mut vec = Point3::new(0);
        match self.axis {
            Axis::X => vec.x = 1,
            Axis::Y => vec.y = 1,
            Axis::Z => vec.z = 1,
        }
        if self.sign == Sign::Neg {
            vec = -vec;
        }
        vec
    }
}

impl Orientation {
    fn new() -> Orientation {
        Orientation {
            direction: SignedAxis {
                axis: Axis::X,
                sign: Sign::Pos,
            },
            rotation: SignedAxis {
                axis: Axis::Y,
                sign: Sign::Pos,
            },
        }
    }

    fn to_rep_point3(&self) -> Point3 {
        self.direction.as_vec3() * 2 + self.rotation.as_vec3()
    }

    #[rustfmt::skip]
    fn from_rep_point3(rep_p3: Point3) -> Self {
        let mut orientation = Orientation::new();

        if rep_p3.x == 2 {
            orientation.direction = SignedAxis { axis: Axis::X, sign: Sign::Pos };
        } else if rep_p3.y == 2 {
            orientation.direction = SignedAxis { axis: Axis::Y, sign: Sign::Pos };
        } else if rep_p3.z == 2 {
            orientation.direction = SignedAxis { axis: Axis::Z, sign: Sign::Pos };
        } else if rep_p3.x == -2 {
            orientation.direction = SignedAxis { axis: Axis::X, sign: Sign::Neg };
        } else if rep_p3.y == -2 {
            orientation.direction = SignedAxis { axis: Axis::Y, sign: Sign::Neg };
        } else if rep_p3.z == -2 {
            orientation.direction = SignedAxis { axis: Axis::Z, sign: Sign::Neg };
        } else {
            panic!("Invalid representative point");
        }

        if rep_p3.x == 1 {
            orientation.rotation = SignedAxis { axis: Axis::X, sign: Sign::Pos };
        } else if rep_p3.y == 1 {
            orientation.rotation = SignedAxis { axis: Axis::Y, sign: Sign::Pos };
        } else if rep_p3.z == 1 {
            orientation.rotation = SignedAxis { axis: Axis::Z, sign: Sign::Pos };
        } else if rep_p3.x == -1 {
            orientation.rotation = SignedAxis { axis: Axis::X, sign: Sign::Neg };
        } else if rep_p3.y == -1 {
            orientation.rotation = SignedAxis { axis: Axis::Y, sign: Sign::Neg };
        } else if rep_p3.z == -1 {
            orientation.rotation = SignedAxis { axis: Axis::Z, sign: Sign::Neg };
        } else {
            panic!("Invalid representative point");
        }

        orientation
    }

    fn as_index(&self) -> usize {
        ALL_POSSIBLE_ORIENTATIONS
            .iter()
            .position(|&x| x == *self)
            .unwrap()
    }

    fn from_index(index: usize) -> Self {
        ALL_POSSIBLE_ORIENTATIONS[index]
    }
}

#[derive(Debug)]
enum DetectedBeacons {
    RawScan(Vec<Point3>),
    CorrectOrientation(HashSet<Point3>),
    AllOrientations(Vec<Vec<Point3>>),
}

#[derive(Debug)]
struct Scanner {
    id: u32,
    position: Option<Point3>,
    orientation: Option<Orientation>,
    detected_beacons: DetectedBeacons,
}

fn parse_file(path: &Path) -> Vec<Scanner> {
    let file = File::open(&path).expect(&format!("couldn't open {}", path.display()));
    let lines = io::BufReader::new(file).lines();

    let mut scanners: Vec<Scanner> = Vec::new();

    for line in lines {
        let line = line.unwrap();

        if line.starts_with("---") {
            scanners.push(Scanner {
                id: scanners.len() as u32,
                position: None,
                orientation: None,
                detected_beacons: DetectedBeacons::RawScan(Vec::new()),
            });
        } else if !line.is_empty() {
            let current_scanner = scanners
                .last_mut()
                .expect("malformed input: should start with scanner");

            if let DetectedBeacons::RawScan(detected_beacons) =
                &mut current_scanner.detected_beacons
            {
                detected_beacons.push(Point3::from_iter(
                    line.split(',').map(|x| x.parse::<i32>().unwrap()),
                ));
            }
        }
    }

    scanners
}

fn rotate_beacon_direction<F>(
    org_orientation: Orientation,
    old_beacon_positions: &Vec<Point3>,
    new_beacon_positions: &mut [Vec<Point3>],
    roation_function: F,
) -> Orientation
where
    F: Fn(Point3) -> Point3,
{
    let new_orientation =
        Orientation::from_rep_point3(roation_function(org_orientation.to_rep_point3()));

    for i in 0..4 {
        let mut new_or_rep_point3 = new_orientation.to_rep_point3();
        for _ in 0..i {
            new_or_rep_point3.rotate_over(new_orientation.direction);
        }
        assert_eq!(
            new_orientation.as_index() + i,
            Orientation::from_rep_point3(new_or_rep_point3).as_index()
        );

        for &beacon_position in old_beacon_positions {
            let mut new_pos = roation_function(beacon_position);
            for _ in 0..i {
                new_pos.rotate_over(new_orientation.direction);
            }
            new_beacon_positions[i as usize].push(new_pos);
        }
    }

    new_orientation
}

fn rel_beacon_positions_in_all_orientations(
    rel_beacon_positions: &Vec<Point3>,
) -> Vec<Vec<Point3>> {
    let mut rel_beacon_positions_in_all_orientations: Vec<Vec<Point3>> = Vec::new();
    rel_beacon_positions_in_all_orientations.resize(24, Vec::new());

    let dir = Orientation::new();
    assert_eq!(dir.as_index(), 0);

    // +X
    let new_dir = rotate_beacon_direction(
        dir,
        rel_beacon_positions,
        rel_beacon_positions_in_all_orientations.as_mut_slice(),
        |x| x,
    );
    assert_eq!(new_dir.as_index(), 0);

    // +Y
    let (old, new) = rel_beacon_positions_in_all_orientations.split_at_mut(4);
    let new_dir = rotate_beacon_direction(dir, &old[0], new, |mut x| {
        *(x.rotate_over(SignedAxis {
            axis: Axis::Z,
            sign: Sign::Pos,
        }))
    });
    assert_eq!(new_dir.as_index(), 4);

    // +Z
    let (old, new) = rel_beacon_positions_in_all_orientations.split_at_mut(8);
    let new_dir = rotate_beacon_direction(dir, &old[0], new, |mut x| {
        *(x.rotate_over(SignedAxis {
            axis: Axis::Y,
            sign: Sign::Neg,
        }))
    });
    assert_eq!(new_dir.as_index(), 8);

    // -X
    let (old, new) = rel_beacon_positions_in_all_orientations.split_at_mut(12);
    let up_dir = Orientation {
        direction: SignedAxis {
            axis: Axis::Y,
            sign: Sign::Pos,
        },
        rotation: SignedAxis {
            axis: Axis::X,
            sign: Sign::Neg,
        },
    };
    let new_dir = rotate_beacon_direction(up_dir, &old[up_dir.as_index()], new, |mut x| {
        *(x.rotate_over(SignedAxis {
            axis: Axis::Z,
            sign: Sign::Pos,
        }))
    });
    assert_eq!(new_dir.as_index(), 12);

    // -Y
    let (old, new) = rel_beacon_positions_in_all_orientations.split_at_mut(16);
    let new_dir = rotate_beacon_direction(dir, &old[0], new, |mut x| {
        *(x.rotate_over(SignedAxis {
            axis: Axis::Z,
            sign: Sign::Neg,
        }))
    });
    assert_eq!(new_dir.as_index(), 16);

    // -Z
    let (old, new) = rel_beacon_positions_in_all_orientations.split_at_mut(20);
    let new_dir = rotate_beacon_direction(dir, &old[0], new, |mut x| {
        *(x.rotate_over(SignedAxis {
            axis: Axis::Y,
            sign: Sign::Pos,
        }))
    });
    assert_eq!(new_dir.as_index(), 20);

    assert_eq!(rel_beacon_positions_in_all_orientations.len(), 24);

    rel_beacon_positions_in_all_orientations
}

/// If the unknown scanner can be solved from the known, will return the scanner's absolute position and orientation index
fn try_solve_scanner_from_known(known: &Scanner, unknown: &Scanner) -> Option<(Point3, usize)> {
    if let DetectedBeacons::CorrectOrientation(known_beacons) = &known.detected_beacons {
        if let DetectedBeacons::AllOrientations(unknown_beacons) = &unknown.detected_beacons {
            for (orientation_index, rotation) in unknown_beacons.iter().enumerate() {
                for known_root_beacon in known_beacons {
                    for unknown_root_beacon in rotation.iter().skip(12 - 1) {
                        let mut count = 0;
                        for unknown_beacon in rotation.iter() {
                            let absolute_unknown_beacon_position =
                                (*unknown_beacon) - (*unknown_root_beacon) + (*known_root_beacon);
                            if known_beacons.contains(&absolute_unknown_beacon_position) {
                                count += 1;
                            }
                        }
                        if count >= 12 {
                            return Some((
                                (*known_root_beacon) - (*unknown_root_beacon),
                                orientation_index,
                            ));
                        }
                    }
                }
            }
        } else {
            panic!()
        }
    } else {
        panic!()
    }
    return None;
}

fn main() {
    println!("Loading input...");
    let mut scanners = parse_file(Path::new("../../day19_input.txt"));
    let num_scanners = scanners.len();

    // Get all orientations for every scanner
    println!("Calculating possible beacon arrangements based on scanner orientations...");
    for scanner in scanners.iter_mut().skip(1) {
        if let DetectedBeacons::RawScan(detected_beacons) = &scanner.detected_beacons {
            scanner.detected_beacons = DetectedBeacons::AllOrientations(
                rel_beacon_positions_in_all_orientations(&detected_beacons),
            );
        } else {
            panic!();
        }
    }

    // Figure out the absolute positions & orientations of the scanners (relative to the first scanner)
    println!("Solving scanner positions and orientations...");
    let mut first_scanner = scanners.remove(0);
    if let DetectedBeacons::RawScan(detected_beacons) = first_scanner.detected_beacons {
        first_scanner.detected_beacons = DetectedBeacons::CorrectOrientation(
            std::iter::FromIterator::from_iter(detected_beacons),
        );
        first_scanner.position = Some(Point3::new(0));
        first_scanner.orientation = Some(Orientation::new());
    } else {
        panic!();
    }

    let mut solved_scanners = vec![first_scanner];

    for i in 0.. {
        if solved_scanners.get(i).is_none() {
            break;
        }
        for j in (0..scanners.len()).rev() {
            let scanner = &mut scanners[j];

            if let Some((scanner_position, orientation_index)) =
                try_solve_scanner_from_known(solved_scanners.get(i).unwrap(), &scanner)
            {
                scanner.position = Some(scanner_position);
                let mut beacons: HashSet<Point3> = HashSet::new();
                if let DetectedBeacons::AllOrientations(beacons_in_all_orientations) =
                    &scanner.detected_beacons
                {
                    let relative_beacons = &beacons_in_all_orientations[orientation_index];
                    for rb in relative_beacons {
                        beacons.insert((*rb) + scanner_position);
                    }
                } else {
                    panic!();
                }
                scanner.detected_beacons = DetectedBeacons::CorrectOrientation(beacons);

                println!(
                    "({}/{}) Solved scanner {} with scanner {}. Absolute position: {:?}",
                    solved_scanners.len() + 1,
                    num_scanners,
                    scanner.id,
                    solved_scanners.get(i).unwrap().id,
                    scanner.position
                );

                solved_scanners.push(scanners.remove(j));
            }
        }
    }

    println!("Combining scanner beacon sets...");
    let mut all_beacons: HashSet<Point3> = HashSet::new();
    for scanner in &solved_scanners {
        if let DetectedBeacons::CorrectOrientation(beacons) = &scanner.detected_beacons {
            all_beacons.extend(beacons);
        } else {
            panic!();
        }
    }

    let mut greatest_manhattan_distance = 0;
    for (i, s1) in solved_scanners.iter().enumerate() {
        for s2 in solved_scanners.iter().skip(i + 1) {
            greatest_manhattan_distance = std::cmp::max(
                greatest_manhattan_distance,
                s1.position
                    .unwrap()
                    .manhattan_distance(s2.position.unwrap()),
            );
        }
    }

    println!("");
    println!("Total Beacons: {}", all_beacons.len());
    println!(
        "Greatest Manhattan distance: {}",
        greatest_manhattan_distance
    );
}
