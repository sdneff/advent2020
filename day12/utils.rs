use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use std::mem;

use self::Direction::*;
use self::Rotation::*;
use self::Action::*;

#[derive(Debug)]
pub enum Direction {
    N,
    W,
    S,
    E
}

impl Direction {
    pub fn rotate(&self, rotation: &Rotation) -> Direction {
        match *self {
            N => match rotation {
                Left => W,
                Right => E,
                AboutFace => S
            },
            W => match rotation {
                Left => S,
                Right => N,
                AboutFace => E
            },
            S => match rotation {
                Left => E,
                Right => W,
                AboutFace => N
            },
            E => match rotation {
                Left => N,
                Right => S,
                AboutFace => W
            }
        }
    }

    pub fn as_step(&self) -> (i32, i32) {
        match *self {
            N => (0, 1),
            E => (1, 0),
            S => (0, -1),
            W => (-1, 0)
        }
    }
}

#[derive(Debug)]
pub enum Rotation {
    Left,
    Right,
    AboutFace
}

#[derive(Debug)]
pub enum Action {
    Rotate(Rotation),
    MoveForward(i32),
    MoveDirection(Direction, i32)
}

#[derive(Debug)]
pub struct Position {
    pub x: i32,
    pub y: i32,
    pub facing: Direction
}

impl Position {
    pub fn move_by(&mut self, action: &Action) {
        match action {
            Rotate(rotation) => self.facing = self.facing.rotate(&rotation),
            MoveForward(steps) => {
                let (dx, dy) = self.facing.as_step();
                self.x += dx * steps;
                self.y += dy * steps;
            }
            MoveDirection(dir, steps) => {
                let (dx, dy) = dir.as_step();
                self.x += dx * steps;
                self.y += dy * steps;
            }
        }
    }

    pub fn move_to_waypoint(&mut self, waypoint: &Position, count: i32) {
        self.x += waypoint.x * count;
        self.y += waypoint.y * count;
    }

    pub fn rotate_around_origin(&mut self, rot: &Rotation) {
        match rot {
            Left => {
                mem::swap(&mut self.x, &mut self.y);
                self.x *= -1;
            }
            Right => {
                mem::swap(&mut self.x, &mut self.y);
                self.y *= -1;
            }
            AboutFace => {
                self.x *= -1;
                self.y *= -1;
            }
        }
    }
}

// parsing, I/O

pub fn parse_action(s: &String) -> Option<Action> {
    let (action, num) = s.split_at(1);
    if let Some(value) = num.parse().ok() {
        match action {
            "N" => Some(MoveDirection(N, value)),
            "W" => Some(MoveDirection(W, value)),
            "S" => Some(MoveDirection(S, value)),
            "E" => Some(MoveDirection(E, value)),
            "F" => Some(MoveForward(value)),
            "R" => match value {
                90 => Some(Rotate(Right)),
                180 => Some(Rotate(AboutFace)),
                270 => Some(Rotate(Left)),
                _ => None
            }
            "L" => {
                match value {
                    90 => Some(Rotate(Left)),
                    180 => Some(Rotate(AboutFace)),
                    270 => Some(Rotate(Right)),
                    _ => None
                }
            }
            _ => None
        }
    } else {
        None
    }
}

pub fn load_navigation(s: &str) -> Vec<Action> {
    let f = File::open(s).expect("file not found");

    return BufReader::new(f)
        .lines()
        .filter_map(Result::ok)
        .map(|l| {
            parse_action(&l).expect("invalid action")
        })
        .collect();
}
