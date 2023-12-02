import Aoc2023.Common.IO
import Aoc2023.Common.Parsec
import Aoc2023.Common.Util

namespace Day2

inductive Color where
  | Blue | Red | Green
deriving Repr, BEq

def colorParser : Parsec Color := do
  let s ← Parsec.pstring "blue" <|> Parsec.pstring "red" <|> Parsec.pstring "green" <|> Parsec.manyChars (Parsec.anyChar)
  match s with
  | "blue" => return .Blue
  | "red" => return .Red
  | "green" => return .Green
  | s => Parsec.fail s!"Unknown color:{s}"

abbrev Turn := List (Color × Nat)

partial def turnCore (acc : Array (Color × Nat)) : Parsec Turn := do
  Parsec.ws
  let num ← Parsec.nat
  Parsec.ws
  let c ← colorParser
  let acc' := acc.push ⟨ c , num ⟩
  let p ← Parsec.peek?
  match p with
  | .some ',' =>
      Parsec.skip
      turnCore acc'
  | _ => return acc'.toList



def turnParser : Parsec Turn := turnCore #[]

#eval Parsec.run turnParser "6 red, 5 blue"

structure Game where
  turns : List Turn
  idx : Nat
deriving Repr

partial def gameCore (acc : Array Turn) : Parsec (List Turn) := do
  let turn ← turnParser
  let acc' := acc.push turn
  let p ← Parsec.peek?
  match p with
  | .some ';' =>
      Parsec.skip
      gameCore acc'
  | _ => return acc'.toList

def gameParser : Parsec Game := do
  let _ ← Parsec.pstring "Game "
  let idx ← Parsec.nat
  let _ ← Parsec.pchar ':'
  let turns ← gameCore #[]
  return ⟨ turns, idx ⟩

#eval Parsec.run gameParser "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

def turnPossible (turn : Turn) : Bool :=
  turn.all (fun ⟨ c , n ⟩ => (c == .Blue && n ≤ 14) || (c == .Green && n ≤ 13) ||   (c == .Red && n ≤ 12))

def gamePossible (game : Game) : Bool :=
  not (game.turns.any (fun turn => not (turnPossible turn)))


#eval gamePossible { turns := [[(Day2.Color.Blue, 3), (Day2.Color.Red, 4)],
            [(Day2.Color.Red, 1), (Day2.Color.Green, 2), (Day2.Color.Blue, 6)],
            [(Day2.Color.Green, 2)]], idx := 1 }

def power (game : Game) : Nat :=
  let maxGreen := (game.turns.map (fun turn =>
    ((turn.filter (fun t => t.1 == .Green)).map (fun t => t.2)).maximum?.getD 0)).maximum?.getD 0
  let maxBlue := (game.turns.map (fun turn =>
    ((turn.filter (fun t => t.1 == .Blue)).map (fun t => t.2)).maximum?.getD 0)).maximum?.getD 0
  let maxRed := (game.turns.map (fun turn =>
    ((turn.filter (fun t => t.1 == .Red)).map (fun t => t.2)).maximum?.getD 0)).maximum?.getD 0
  maxGreen*maxBlue*maxRed

def value (xs : String) : Nat :=
  match Parsec.run gameParser xs with
  | .ok game => if gamePossible game then game.idx else 0
  | .error _ => 0

#eval value "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
#eval value "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"

def powerS (xs : String) : Nat :=
  match Parsec.run gameParser xs with
  | .ok game => power game
  | .error _ => 0


def processLines (xs : List String) : Nat :=
  Util.sum (xs.map value)

def processLines2 (xs : List String) : Nat :=
  Util.sum (xs.map powerS)

#eval processLines ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"]
def day2part1 : IO Unit := do
  let stdin ← IO.getStdin
  let lines ← IO.extractLines stdin
  let stdout ← IO.getStdout
  let result := processLines lines
  stdout.putStrLn s!"{result}"

def day2part2 : IO Unit := do
  let stdin ← IO.getStdin
  let lines ← IO.extractLines stdin
  let stdout ← IO.getStdout
  let result := processLines2 lines
  stdout.putStrLn s!"{result}"
