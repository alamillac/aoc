import simplifile

pub fn get_input_filename( day: String) -> String {
  let assert Ok(result) = simplifile.read("../../inputs/" <> day <> ".txt")
  result
}
