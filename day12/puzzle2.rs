mod utils;

use utils::*;
use utils::Direction::*;
use utils::Action::*;

fn main() {
    let actions = load_navigation("./navigation.txt");

    let mut position = Position {
        x: 0,
        y: 0,
        facing: E
    };

    let mut waypoint = Position {
        x: 10,
        y: 1,
        facing: E // meaningless
    };

    for action in actions.iter() {
        match action {
            Rotate(rotation) => waypoint.rotate_around_origin(rotation),
            MoveForward(steps) => position.move_to_waypoint(&waypoint, *steps),
            MoveDirection(_, _) => waypoint.move_by(action)
        }
    }

    println!("Final position: {:?}", position);
    println!("Manhattan distance: {}", position.x.abs() + position.y.abs());

}
