# Tom's Advent of Code Workspace

All my challenge solutions over the years - all written in haskell.

# Cheatsheet

## Haskell syntax stuff

- `$` - removing brackets ("generally lower precedence than stuff on the right")
- `let .. in` - bind values in expression (`where` is related)
- `<$>` / `fmap` - mapping over things that are Functor
- `<*>` / `ap` - mapping over things that are Applicative Functor (mapping function lifted to context)
- `pure` - lift pure value into monad context
- `>>=` - map over value held within monad context (`do`, `let` and `<-` notation un-nests this)

## Lists (Data.List / Data.Foldable)

- `foldr` / `foldl` / `foldl'` - reduce a structure to a single value (foldl' is strict, prevents stack overflow)
- `unfoldr` - build a list from a seed value using a function (dual of fold)
- `scanl` / `scanr` - like fold but returns list of intermediate results
- `filter` - keep only elements matching predicate
- `any` / `all` - check if any/all elements satisfy predicate
- `concat` / `concatMap` - flatten nested lists / map then flatten (`join` more general)
- `transpose` - swap rows and columns (great for grids)
- `group` - group consecutive equal elements into sublists
- `sort` / `sortBy` / `sortOn` - sort a list
- `nub` - remove duplicates (`Set.fromList` better?)
- `subsequences` - all subsets of a list
- `permutations` - all orderings of a list
- `zip` / `zipWith` - combine two lists element-wise
- `partition` - split list into tuple of (matching, non-matching)
- `find` - first element matching predicate
- `elemIndex` - index of first occurrence
- `cycle` - turn finite list into infinitely repeating one
- `length` / `sum` / `product` / `maximum` / `minimum` - common aggregations
- `break` / `span` - split list at first element that fails/satisfies predicate
- `takeWhile` / `dropWhile` - take/drop elements while predicate holds
- `splitAt` - split at index: `splitAt n xs = (take n xs, drop n xs)`
- `repeat` - infinite list of same value
- `replicate` - finite list of same value: `replicate n x`
- `iterate` - generate infinite list by repeatedly applying function: `iterate f x = [x, f x, f (f x), ...]`

## Sets (Data.Set)

- `empty` - construct empty Set
- `fromList` / `toList` - convert between lists and sets
- `member` - check if element exists
- `insert` / `delete` - add/remove elements
- `union` / `intersection` / `difference` - set operations
- `size` - number of elements
- `null` - check if set is empty

## Maps (Data.Map)

- `empty` - construct empty Map
- `fromList` / `toList` - convert between lists and maps `[(k,v)]`
- `lookup` - find value by key
- `member` - check if key exists
- `insert` / `delete` - add/remove key-value pairs
- `adjust` / `update` - modify value at a key
- `insertWith` - insert with function to combine values if key exists
- `union` / `intersection` / `difference` - map operations
- `keys` / `elems` - get all keys or all values
- `size` - number of entries

## Vectors (Data.Vector)

- `empty` - construct empty Vector
- `fromList` / `toList` - convert between lists and vectors
- `(!)` - O(1) index access with bounds check
- `(!?)` - O(1) index access returning Maybe
- `length` - number of elements
- `head` / `tail` / `last` / `init` - get parts of vector
- `map` / `filter` / `foldl` / `foldr` - standard operations (but O(1) indexing!)
- `generate` - create vector from index function: `generate n f`
- `replicate` - create vector with repeated value
- `cons` / `snoc` - add element to front/back
- `slice` - extract subvector: `slice start length vec`
- `update` - modify element at index (functional, returns new vector)
- `//` - batch update: `vec // [(index, newValue), ...]`

## Parsing (Megaparsec)

- `many` / `some` - zero or more / one or more matches
- `sepBy` / `sepBy1` - parse list separated by delimiter (e.g. "1,2,3")
- `endBy` / `endBy1` - parse list terminated by delimiter (e.g. "1,2,3,")
- `between` - parse something between two parsers (e.g., parentheses)
- `choice` - try a list of parsers in order, return first success (can use `<|>` too)
- `try` - backtrack if parser fails (needed for ambiguity - e.g. choice [string "tom", string "tim"])
- `char` / `string` - match specific character or string
- `digitChar` / `letterChar` / `alphaNumChar` / `spaceChar` - single character parsers
- `optional` - make a parser optional
- `skipMany` / `skipSome` - consume input without returning it
- `newline` / `eol` - end of line
- `count` - parse exactly n occurrences

## State Monad (Control.Monad.State)

- `get` - retrieve the current state
- `put` - replace the state with a new value
- `modify` - update the state with a function: `modify (+ 1)`
- `gets` - retrieve a value derived from state: `gets fst`
- `runState` - run state computation, returns `(result, finalState)`
- `evalState` - run state computation, returns only the result
- `execState` - run state computation, returns only the final state
- `state` - construct a State action from a function: `state :: (s -> (a, s)) -> State s a`

## Maybe (Data.Maybe)

- `fromMaybe` - provide default for Maybe: `fromMaybe defaultVal maybeVal`
- `mapMaybe` - map and filter Nothings in one go
- `catMaybes` - filter out Nothing values from list
- `isJust` / `isNothing` - check Maybe status
- `maybe` - fold over Maybe: `maybe defaultVal f maybeVal`

## Tuples (Data.Bifunctor)

- `bimap` - map over both elements of tuple: `bimap f g (a, b) = (f a, g b)`
- `first` / `second` - map over first/second element of tuple

## Numeric / Math

- `div` / `mod` - integer division and modulo
- `quot` / `rem` - quotient and remainder (different for negative numbers)
- `gcd` / `lcm` - greatest common divisor / least common multiple
- `until` - apply function until predicate is true: `until p f x`
- `abs` - absolute value
- `signum` - sign of a number (-1, 0, or 1)

## Bitwise Operations (Data.Bits)

- `(.&.)` / `(.|.)` - bitwise AND / OR
- `xor` - bitwise XOR
- `shiftL` / `shiftR` - bit shifts left/right
- `complement` - bitwise NOT

## Search Algorithms

- `dijkstra` (Algorithm.Search) - shortest path in weighted graphs
- `aStar` (Data.Graph.AStar) - A* search algorithm
- `dfs` (Algorithm.Search) - depth-first search
- `bfs` (Algorithm.Search) - breadth-first search

## Memoization

- `memoFix` (Data.MemoTrie) - memoize recursive functions

## Control Flow (Control.Monad)

- `when` / `unless` - conditional monadic actions
- `guard` - pattern guard in monads (fails if False)
- `join` - flatten nested monads: `join :: m (m a) -> m a`
- `mapM` / `mapM_` - map a monadic function over a list
- `forM` / `forM_` - flipped mapM
- `foldM` - monadic fold (useful for early termination with Either/Maybe or stateful folding with State)
- `replicateM` / `replicateM_` - repeat monadic action n times
- `sequence` / `sequence_` - turn `[m a]` into `m [a]` (run actions, collect results)

## Custom Utils (from 2025/app/Handy.hs)

- `xy` - parser combinator that returns zero-indexed (x, y) position
- `num` - parser combinator for reading integers
- `parse` / `parse'` - run parser or die with colored error messages
- `puzzle` - fetch puzzle input (downloads and caches from adventofcode.com)
- `chunkBy` - chunk list by predicate: groups consecutive elements where `p a b` is True

---

# Common Patterns

## Frequency Map

Build a frequency count map from a list.

```haskell
freqmap :: Ord a => [a] -> Map a Int
freqmap = foldr (\e -> Map.insertWith (+) e 1) Map.empty
```

## Early Termination with foldM and Either

Use `foldM` with `Either` to fold until a condition is met, then short-circuit with `Left`.

```haskell
let Left answer = foldM (\state item ->
                            if ? then Left <answer>
                                 else Right <next_state>
                        ) <start_state> items
```

## Memoizing Recursive Functions with memoFix

Use `memoFix` to automatically memoize a recursive function by passing `rec` as a parameter.

```haskell
count :: (Int, Int) -> Int
count = memoFix $ \rec (depth, val) ->
    if depth == maxDepth
        then 1
        else rec (depth + 1, newVal)  -- Call rec for memoized recursion
```

## Memoizing Non-Recursive Functions with memo

Use `memo` to cache results of non-recursive functions.

```haskell
expensive :: Int -> Int
expensive = memo $ \n -> 
    -- expensive computation here
    sum [1..n] ^ 2
```

## State Monad for Flood Fill / Graph Traversal

Use State monad to track visited nodes during recursive exploration.

```haskell
flood :: Grid -> XY -> State (Set XY) (Set XY)
flood grid xy = do
    modify (Set.insert xy)  -- Mark as visited
    visited <- get
    -- Recursively flood unvisited neighbors
    neighbors <- mapM (\neighbor -> 
        if neighbor `Set.notMember` visited
            then flood grid neighbor
            else pure Set.empty
        ) (neighbours grid xy)
    pure $ Set.insert xy $ Set.unions neighbors

-- Run the flood fill
result = evalState (flood grid startPos) Set.empty
```

## BFS / DFS Search

Use DFS or BFS from Algorithm.Search to find paths or explore graphs.

```haskell
import Algorithm.Search (dfs, bfs)

-- Find a path:
--  DFS - depth first   - not necessarily shortest, but maybe quicker to find a valid solution
--  BFS - breadth first - find shortest, but slower as more exhaustive
findPath :: Node -> Maybe [Node]
findPath start = 
    [dfs / bfs]
        neighbours -- (node -> [node]) - get adjacent nodes
        goal       -- (node -> Bool) - have we reached the goal?
        start      -- starting node
```

## A* Pathfinding

Use A* algorithm for optimal pathfinding with a heuristic function.

```haskell
import Data.Graph.AStar (aStar)
import qualified Data.HashSet as HashSet

findPath :: Grid -> XY -> XY -> Maybe [XY]
findPath grid start end = 
    aStar
        neighbours -- (node -> HashSet node) - get valid neighbors
        cost       -- (node -> node -> Int) - actual cost between nodes
        heuristic  -- (node -> Int) - estimated cost to goal (e.g. manhattan distance)
        goal       -- (node -> Bool) - check if we've reached the goal
        start      -- starting node
```
