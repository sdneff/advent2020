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


pub fn get_closest_seat_positions(layout: &Vec<Vec<Position>>, i: usize, j: usize) -> Vec<&Position> {
    let mut visibles = Vec::<&Position>::new();

    for (dx, dy) in Direction::iterator().map(|d| d.get_offset()) {
        let mut x = i as i32;
        let mut y = j as i32;

        loop {
            x += dx;
            y += dy;
            if let Some(p) = read_position(layout, x, y) {
                match p {
                    Seat(_) => {
                        visibles.push(p);
                        break
                    },
                    Floor => continue
                }
            } else {
                break
            }
        }
    }

    visibles
}

fn get_next_position_state(layout: &Vec::<Vec::<Position>>, r: usize, c: usize) -> (bool, Position) {
    match *read_position(layout, r as i32, c as i32).unwrap() {
        Floor => (false, Floor),
        Seat(occupied) => {
            let visibles = count_occupied(&get_closest_seat_positions(layout, r, c));
            if occupied && visibles >= 5 {
                (true, Seat(false))
            } else if !occupied && visibles == 0 {
                (true, Seat(true))
            } else {
                (false, Seat(occupied))
            }
        }
    }
}
