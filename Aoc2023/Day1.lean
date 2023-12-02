import Aoc2023.Common.Parsec

def hello := "World"




partial def extractLines (stream : IO.FS.Stream): IO (List String) := do
  let line ← stream.getLine
  if line.isEmpty then
    return []
  else
    let tail ← extractLines stream
    return line :: tail




def Char.Zero : Char := ⟨ 0x30 , by sorry ⟩
def Char.Nine : Char := ⟨ 0x39 , by sorry ⟩
theorem char_zero : Char.Zero.ToDigit = 0 := by rfl
theorem char_nine : Char.Nine.ToDigit = 9 := by rfl

def List.startsWith [BEq α] (haystack : List α) (needle: List α) : Bool :=
  match haystack, needle with
  | x :: xs, y :: ys => if x == y then xs.startsWith ys else false
  | _ , [] => true
  | _ , _ => false

def maybeNumber (xs : List Char) : Option Nat :=
  match xs with
  | 'o' :: ('n' :: ('e' :: _)) => .some 1
  | 't' :: ('w' :: ('o' :: _)) => .some 2
  | 't' :: ('h' :: ('r' :: ('e' :: ('e' :: _)))) => .some 3
  | 'f' :: ('o' :: ('u' :: ('r' :: _))) => .some 4
  | 'f' :: ('i' :: ('v' :: ('e' :: _))) => .some 5
  | 's' :: ('i' :: ('x' :: _)) => .some 6
  | 's' :: ('e' :: ('v' :: ('e' :: ('n' :: _)))) => .some 7
  | 'e' :: ('i' :: ('g' :: ('h' :: ('t' :: _)))) => .some 8
  | 'n' :: ('i' :: ('n' :: ('e' :: _))) => .some 9
  | _ => .none

def charsToNats (xs : List Char) : List Nat :=
  match xs with
  | [] => ([] : List Nat)
  | x :: xs =>
    if x.isDigit then
      x.ToDigit :: charsToNats xs
    else
      match maybeNumber (x :: xs) with
      | .none => charsToNats xs
      | .some d => d :: charsToNats xs

theorem charsToNatsSingle : charsToNats [Char.Zero] = [0] := by rfl

theorem charsToNatsDouble : charsToNats [Char.Zero, Char.Nine] = [0, 9] := by rfl

theorem charsToNatsTriple : charsToNats [Char.Zero, ⟨ 0x00 , by sorry ⟩, Char.Nine] = [0, 9] := by rfl


def natsToAnswer (xs : List Nat) : Nat :=
  match xs with
  | [] => 0
  | x :: xs => 10 * x + (x :: xs).getLast (by simp)

def processLine (xs : String) : Nat :=
  natsToAnswer (charsToNats xs.data)

def processLines (xs : List String) : Nat :=
  (xs.map processLine).foldr (. + .) 0


def day1part1 : IO Unit := do
  let stdin ← IO.getStdin
  let lines ← extractLines stdin
  let stdout ← IO.getStdout
  let result := processLines lines
  stdout.putStrLn s!"{result}"
