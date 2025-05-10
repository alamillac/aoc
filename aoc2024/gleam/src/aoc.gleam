import gleam/io
import util

pub fn main() {
  io.println("Hello from aoc!")
  let x = util.get_input_filename("8")
  echo x
}
