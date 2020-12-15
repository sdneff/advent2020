mod utils;

use utils::*;
use utils::Direction::*;

fn main() {
    let actions = load_navigation("./navigation.txt");

    let mut position = Position {
        x: 0,
        y: 0,
        facing: E
    };

    for action in actions.iter() {
        position.move_by(action);
    }

    println!("Final position: {:?}", position);
    println!("Manhattan distance: {}", position.x.abs() + position.y.abs());
}
