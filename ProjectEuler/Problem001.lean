module

/-
If we list all the natural numbers below `10` that are multiples of `3` or `5`, we get `3, 5, 6` and `9`. The sum of these multiples is `23`.
Find the sum of all the multiples of `3` or `5` below `1000`.
-/

def multipleOf
  [Mod α] [BEq α] [Zero α]
  (denominator n : α)
  : Bool :=
  n % denominator == 0

instance Function.instHor
  {α : Type u}
  : HOr (α → Bool) (α → Bool) (α → Bool) where
  hOr f g x := f x || g x

def sumOfMultiplesOf3And5Below
  (n : Nat)
  : Nat :=
  (Array.range n)
    |>.filter (multipleOf 3 ||| multipleOf 5)
    |>.foldl (· + ·) 0

#eval sumOfMultiplesOf3And5Below 1000
