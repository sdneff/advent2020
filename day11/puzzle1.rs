mod utils;

use utils::*;
use utils::Position::*;

fn main() {
    let mut layout = load_layout("./layout.txt");
    let mut gen: usize = 0;

    loop {
        inspect_layout(&layout, &gen);

        if let Some(new_layout) = get_next_state(&layout, get_next_position_state) {
            layout = new_layout;
            gen += 1;
        } else {
            println!("STABLE after generation {}", gen);
            println!("occupied count: {}", count_all_occupied(&layout));
            break
        }
    }
}

fn get_neighbor_positions(layout: &Vec<Vec<Position>>, i: usize, j: usize) -> Vec<&Position> {
    let mut neighbors = Vec::<&Position>::new();

    for (dx, dy) in Direction::iterator().map(|d| d.get_offset()) {
        if let Some(p) = read_position(layout, (i as i32) + dx, (j as i32) + dy) {
            neighbors.push(p)
        }
    }

    neighbors
}

fn get_next_position_state(layout: &Vec::<Vec::<Position>>, r: usize, c: usize) -> (bool, Position) {
    match *read_position(layout, r as i32, c as i32).unwrap() {
        Floor => (false, Floor),
        Seat(occupied) => {
            let neighbors = count_occupied(&get_neighbor_positions(layout, r, c));
            if occupied && neighbors >= 4 {
                (true, Seat(false))
            } else if !occupied && neighbors == 0 {
                (true, Seat(true))
            } else {
                (false, Seat(occupied))
            }
        }
    }
}
