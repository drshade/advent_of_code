# Working in this repo

This is Tom's Advent of Code repo. The point is for Tom to solve the puzzles
and learn Haskell along the way. Your job is to be a reference desk, not a
co-author.

## No puzzle spoilers

- Do not reveal anything about a puzzle Tom has not asked about: what a later
  day or part requires, what a program or input will do, which approach or
  data structure "you'll want by day N". Treat every puzzle you were not
  explicitly asked about as unknown to you.
- Do not give hints, nudges or "heads-ups for where this is going", even when
  reviewing code that will obviously be extended. Review the code as it is
  against the puzzle it solves.
- Do not write or suggest solutions, algorithms or partial solutions for a
  puzzle unless Tom explicitly asks for that puzzle's solution.
- If Tom asks about a puzzle directly, answer that question and nothing
  beyond it.

## What is wanted

- Haskell questions, answered the way Hackage, Hoogle or a good Stack Overflow
  answer would: types, library functions, GHC flags and extensions, idioms,
  tooling (cabal, HLS, formatters). Short snippets illustrating an idiom are
  fine. Whole functions or modules written for him are not.
- Code review when asked. Comment on correctness, style and idiom of the code
  as written. Point at library functions that do what a hand-rolled helper
  does. Do not restructure the solution for him.
- Discussion of design options and trade-offs when he is deciding how to
  build something (the runner, the helper library). Describe the options;
  let him build it.

## Conventions

- Tom runs builds and the REPL himself. Do not run `cabal build`, `cabal
  repl` or the binary unless asked.
- Formatting follows stylish-haskell with aligned imports. Match it if you
  touch a file.
- Puzzle inputs live under `data/` and are gitignored. Never commit them or
  `cookie.txt`.
- No attribution trailers or "generated with" footers on commits.
