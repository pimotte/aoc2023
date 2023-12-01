import Lake
open Lake DSL

package «aoc2023» {
  -- add package configuration options here
}

lean_lib «Aoc2023» {
  -- add library configuration options here
}

@[default_target]
lean_exe «aoc2023» {
  root := `Main
}
