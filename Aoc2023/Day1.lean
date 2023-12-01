

def hello := "World"





partial def extractLines (stream : IO.FS.Stream): IO (List String) := do
  let line ← stream.getLine
  if line.isEmpty then
    return []
  else
    let tail ← extractLines stream
    return line :: tail

def Char.ToDigit : Char → Nat := λ char => char.toNat - 48

def Char.Zero : Char := ⟨ 0x30 , by sorry ⟩
def Char.Nine : Char := ⟨ 0x39 , by sorry ⟩
theorem char_zero : Char.Zero.ToDigit = 0 := by rfl
theorem char_nine : Char.Nine.ToDigit = 9 := by rfl

def charsToNats (xs : List Char) : List Nat :=
  match xs with
  | [] => ([] : List Nat)
  | x :: xs =>
    if x.isDigit then
      x.ToDigit :: charsToNats xs
    else
      charsToNats xs

theorem charsToNatsSingle : charsToNats [Char.Zero] = [0] := by rfl

theorem charsToNatsDouble : charsToNats [Char.Zero, Char.Nine] = [0, 9] := by rfl

theorem charsToNatsTriple : charsToNats [Char.Zero, ⟨ 0x00 , by sorry ⟩, Char.Nine] = [0, 9] := by rfl

-- partial def processLineHelper (first : Option Nat) (last : Option Nat) (xs : String) : Option Nat :=
--   match xs.data with
--   | [] => do
--     let f ← first
--     let l ← first
--     return f*10 + l
--   | head :: tail =>
--     if head.isDigit then
--       match first with
--       | .none => processLineHelper (.some head.toNat) (.some head.toNat) (.mk tail)
--       | firstDigit => processLineHelper firstDigit (.some head.toNat) (.mk tail)
--     else
--       processLineHelper first last (.mk tail)

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
