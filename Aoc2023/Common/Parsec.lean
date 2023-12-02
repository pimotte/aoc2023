

namespace Parsec
inductive ParseResult (α : Type) where
  | success (pos : String.Iterator) (res : α)
  | error (pos : String.Iterator) (err : String)
  deriving Repr
end Parsec

def Parsec (α : Type) : Type := String.Iterator → Parsec.ParseResult α

def Char.ToDigit : Char → Nat := λ char => char.toNat - 48

namespace Parsec

@[inline]
protected def pure (a : α) : Parsec α := λ it =>
 .success it a

@[inline]
def bind {α β : Type} (f : Parsec α) (g : α → Parsec β) : Parsec β := λ it =>
  match f it with
  | .success rem a => g a rem
  | .error pos msg => .error pos msg

instance : Monad Parsec :=
  { pure := Parsec.pure, bind }

@[inline]
def fail (msg : String) : Parsec α := fun it =>
  .error it msg

@[inline]
def orElse (p : Parsec α) (q : Unit → Parsec α) : Parsec α := fun it =>
  match p it with
  | .success rem a => .success rem a
  | .error rem err =>
    if it = rem then q () it else .error rem err

@[inline]
def attempt (p : Parsec α) : Parsec α := λ it =>
  match p it with
  | .success rem res => .success rem res
  | .error _ err => .error it err

instance : Alternative Parsec :=
{ failure := fail "", orElse }

protected def run (p : Parsec α) (s : String) : Except String α :=
  match p s.mkIterator with
  | Parsec.ParseResult.success _ res => Except.ok res
  | Parsec.ParseResult.error it err  => Except.error s!"offset {repr it.i.byteIdx}: {err}"

def pstring (s : String) : Parsec String := λ it =>
  let substr := it.extract (it.forward s.length)
  if substr = s then
    .success (it.forward s.length) substr
  else
    .error it s!"expected: {s}"

@[inline]
def peek? : Parsec (Option Char) := fun it =>
  if it.hasNext then
    .success it it.curr
  else
    .success it none

@[inline]
def skip : Parsec Unit := fun it =>
  .success it.next ()

partial def natCore (acc digits : Nat) : Parsec (Nat × Nat) := do
  let some c ← peek? | return (acc, digits)
  if '0' ≤ c ∧ c ≤ '9' then
    skip
    let acc' := 10*acc + (c.val.toNat - '0'.val.toNat)
    natCore acc' (digits+1)
  else
    return (acc, digits)

def nat : Parsec Nat := do
  let res ← natCore 0 0
  return res.1

partial def skipWs (it : String.Iterator) : String.Iterator :=
  if it.hasNext then
    let c := it.curr
    if c = '\u0009' ∨ c = '\u000a' ∨ c = '\u000d' ∨ c = '\u0020' then
      skipWs it.next
    else
      it
  else
   it


@[inline]
def ws : Parsec Unit := fun it =>
  .success (skipWs it) ()

def unexpectedEndOfInput := "unexpected end of input"

@[inline]
def anyChar : Parsec Char := λ it =>
  if it.hasNext then .success it.next it.curr else .error it unexpectedEndOfInput

@[inline]
def pchar (c : Char) : Parsec Char := attempt do
  if (←anyChar) = c then pure c else fail s!"expected: '{c}'"

@[specialize]
partial def manyCore (p : Parsec α) (acc : Array α) : Parsec $ Array α :=
  (do manyCore p (acc.push $ ←p))
  <|> pure acc

@[inline]
def many (p : Parsec α) : Parsec $ Array α := manyCore p #[]

@[inline]
def many1 (p : Parsec α) : Parsec $ Array α := do manyCore p #[←p]

@[specialize]
partial def manyCharsCore (p : Parsec Char) (acc : String) : Parsec String :=
  (do manyCharsCore p (acc.push $ ←p))
  <|> pure acc

@[inline]
def manyChars (p : Parsec Char) : Parsec String := manyCharsCore p ""
