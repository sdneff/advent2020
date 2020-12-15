use std::convert::TryFrom;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use std::slice::Iter;

use self::Position::*;
use self::Direction::*;

#[derive(Debug)]
pub enum Position {
    Floor,
    Seat(bool)
}

impl Position {
    pub fn as_char(&self) -> char {
        match *self {
            Floor => '.',
            Seat(occupied) => if occupied { '#' } else { 'L' }
        }
    }
}

#[derive(Debug)]
pub enum Direction {
    NW,
    N,
    NE,
    E,
    SE,
    S,
    SW,
    W
}

impl Direction {
    pub fn iterator() -> Iter<'static, Direction> {
        static DIRECTIONS: [Direction; 8] = [NW, N, NE, E, SE, S, SW, W];
        DIRECTIONS.iter()
    }

    pub fn get_offset(&self) -> (i32, i32) {
        match *self {
            NW => (-1, 1),
            N => (0, 1),
            NE => (1, 1),
            E => (1, 0),
            SE => (1, -1),
            S => (0, -1),
            SW => (-1, -1),
            W => (-1, 0)
        }
    }
}

// parsing, I/O

fn parse_position(c: &char) -> Position {
    match c {
        '.' => Floor,
        'L' => Seat(false),
        '#' => Seat(true),
        _ => panic!("unsupported character: {}", c)
    }
}

pub fn load_layout(s: &str) -> Vec<Vec<Position>> {
    let f = File::open(s).expect("file not found");

    return BufReader::new(f)
        .lines()
        .filter_map(Result::ok)
        .map(|r| {
            r.chars()
                .map(|c| parse_position(&c))
                .collect()
        })
        .collect();
}

pub fn inspect_layout(layout: &Vec<Vec<Position>>, gen: &usize) {
    println!("Generation {}", gen);
    for row in layout.iter() {
        println!("{}", row.iter().map(|p| p.as_char()).collect::<String>());
    }
    println!("")
}

// layout logic

pub fn read_position(layout: &Vec::<Vec::<Position>>, r: i32, c: i32) -> Option<&Position> {
    if let (Some(r), Some(c)) = (usize::try_from(r).ok(), usize::try_from(c).ok()) {
        if let Some(row) = layout.get(r) {
            return row.get(c)
        }
    }
    None
}

pub fn get_next_state(layout: &Vec<Vec<Position>>, next_state: impl Fn(&Vec<Vec<Position>>, usize, usize) -> (bool, Position)) -> Option<Vec<Vec<Position>>> {
    let mut new_layout = Vec::<Vec::<Position>>::new();
    let mut changed = false;

    for (r, row) in layout.iter().enumerate() {
        let mut new_row = Vec::<Position>::new();
        for (c, _) in row.iter().enumerate() {
            let (is_changed, new_pos) = next_state(layout, r, c);
            new_row.push(new_pos);
            changed = changed || is_changed;
        }
        new_layout.push(new_row);
    }

    if changed {
        Some(new_layout)
    } else {
        None
    }
}

pub fn count_all_occupied(layout: &Vec<Vec<Position>>) -> usize {
    layout.iter()
        .map(|r| {
            r.iter()
                .filter(|n| match n {
                    Seat(true) => true,
                    _ => false
                })
                .count()
        })
        .sum()
}

pub fn count_occupied(set: &Vec<&Position>) -> usize {
    set.iter()
        .filter(|n| match n {
            Seat(true) => true,
            _ => false
        })
        .count()
}
