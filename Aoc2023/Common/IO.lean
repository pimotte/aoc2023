namespace IO

partial def extractLines (stream : IO.FS.Stream): IO (List String) := do
  let line ← stream.getLine
  if line.isEmpty then
    return []
  else
    let tail ← extractLines stream
    return line :: tail
