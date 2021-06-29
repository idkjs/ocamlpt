open Base;

let or_else = (~f: unit => option('a)) =>
  fun
  | None => f()
  | Some(_) as opt => opt;
