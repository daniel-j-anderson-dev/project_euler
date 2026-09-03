module

import Std.Data.Iterators.Producers.Range

/-
The prime factors of `13195` are `5, 7, 13` and `29`.
What is the largest prime factor of the number `600851475143`?
-/

def largestPrimeFactor
  (n : Nat)
  : Nat :=
  let rec loop
    (fuel n factor greatest : Nat)
    : Nat :=
    match fuel with
    | 0 => greatest
    | fuel + 1 =>
      if n < 2 then
        greatest

      else if factor * factor > n then
        if n > 1 then n.max greatest else greatest

      else if n % factor == 0 then
        loop fuel (n / factor) factor (greatest.max factor)

      else
        let factor' := if factor == 2 then 3 else factor + 2
        loop fuel n factor' greatest

  if n < 2 then
    0
  else
    loop n n 2 1

#eval largestPrimeFactor 600851475143
