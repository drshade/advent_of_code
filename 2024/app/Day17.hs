module Day17 where
import           Control.Monad       (void, when)
import           Control.Monad.Loops (untilM_)
import           Control.Monad.RWS   (RWS, ask, get, put, runRWS)
import           Data.Bits           (xor)
import           Data.Functor        (($>))
import           Data.List           (intercalate, isPrefixOf)
import           Handy
import           Text.Parsec         (anyChar, char, choice, digit, many1,
                                      newline, sepBy, string)

type Literal = Int
data Combo = Lit Literal | RegA | RegB | RegC deriving (Show)
data Op = ADV Combo     -- 0
        | BXL Literal   -- 1
        | BST Combo     -- 2
        | JNZ Literal   -- 3
        | BXC Literal   -- 4
        | OUT Combo     -- 5
        | BDV Combo     -- 6
        | CDV Combo     -- 7
        deriving (Show)

parser :: Parser (Int, Int, Int, [Op], [Int])
parser = do
    a <- string "Register A: " *> (read <$> many1 digit) <* newline
    b <- string "Register B: " *> (read <$> many1 digit) <* newline
    c <- string "Register C: " *> (read <$> many1 digit) <* newline <* newline
    -- We need the original program intact (as [Int]) so grab that first
    program :: [Int] <- string "Program: " *> ((read . (:[]) <$> digit) `sepBy` char ',')
    -- Then parse the program as instructions
    let ops = parse (op `sepBy` char ',') (intercalate "," $ show <$> program)
    pure (a, b, c, ops, program)
    where   op :: Parser Op
            op = choice [ char '0' $> ADV <*> (char ',' *> combo)
                        , char '1' $> BXL <*> (char ',' *> literal)
                        , char '2' $> BST <*> (char ',' *> combo)
                        , char '3' $> JNZ <*> (char ',' *> literal)
                        , char '4' $> BXC <*> (char ',' *> literal)
                        , char '5' $> OUT <*> (char ',' *> combo)
                        , char '6' $> BDV <*> (char ',' *> combo)
                        , char '7' $> CDV <*> (char ',' *> combo)
                        ]
            literal :: Parser Literal
            literal = read . (:[]) <$> digit
            combo :: Parser Combo
            combo = do
                v <- literal
                pure $ if v <= 3 then Lit v else case v of
                    4 -> RegA
                    5 -> RegB
                    6 -> RegC
                    _ -> error "spec lied!"

-- Our CPU
type Register = Int
type ProgramCounter = Int
type Outputs = [Int]
type CPU = RWS [Op] () (ProgramCounter, Outputs, Register, Register, Register)
--             ^    ^     ^
--             |    |     |
--             |    |      - State:  PC, Outputs, Registers
--             |     ------- Writer: ()
--              ------------ Reader: Ops

getCombo :: Combo -> CPU Int
getCombo RegA    = get >>= (\(_, _, a, _, _) -> pure a)
getCombo RegB    = get >>= (\(_, _, _, b, _) -> pure b)
getCombo RegC    = get >>= (\(_, _, _, _, c) -> pure c)
getCombo (Lit v) = pure v

inc :: CPU ()
inc = do (pc, o, a, b, c) <- get
         put (pc + 1, o, a, b, c)

step :: Op -> CPU ()

step (BXL v) = do
    (pc, o, a, b, c) <- get
    void $ put (pc, o, a, b `xor` v, c) *> inc
step (BST combo) = do
    (pc, o, a, b, c) <- get
    v <- getCombo combo
    put (pc, o, a, v `mod` 8, c) *> inc
step (BXC v) = do
    (pc, o, a, b, c) <- get
    put (pc, o, a, b `xor` c, c) *> inc

step (ADV combo) = do
    (pc, o, a, b, c) <- get
    v <- getCombo combo
    put (pc, o, a `div` (2 ^ v), b, c) *> inc
step (BDV combo) = do
    (pc, o, a, b, c) <- get
    v <- getCombo combo
    put (pc, o, a, a `div` (2 ^ v), c) *> inc
step (CDV combo) = do
    (pc, o, a, b, c) <- get
    v <- getCombo combo
    put (pc, o, a, b, a `div` (2 ^ v)) *> inc

step (JNZ v) = do
    (pc, o, a, b, c) <- get
    if a /= 0
        then put (v, o, a, b, c)
        else inc
step (OUT combo) = do
    (pc, o, a, b, c) <- get
    v <- getCombo combo
    put (pc, o ++ [v `mod` 8], a, b, c) *> inc

run :: CPU ()
run = do
    untilM_ (do
                -- Get the pc, next and step the machine
                (pc, _, _, _, _) <- get
                ops <- ask
                let op = ops !! pc
                void $ step op
            )
            (do
                -- Until the pc points past the list of ops
                (pc, _, _, _, _) <- get
                (pc >=) . length <$> ask
            )

part1 :: IO [Int]
part1 = do
    input@(a, b, c, ops, _) <- parse parser <$> getInput Main 2024 17
    let ((), (_, out, _, _, _), ()) = runRWS run ops (0, [], a, b, c)
    pure out


-- Wrong approach for part2 - bruteforcing until we find the program
-- does not work... need to reverse engineer the program and be smarter
-- nahhhhh...
runNoHalt :: [Int] -> CPU ()
runNoHalt original = do
    untilM_
        (do
            (pc, o, a, b, c) <- get
            ops <- ask
            let op = ops !! pc
            void $ step op
        )
        (do
            (pc, o, a, b, c) <- get
            ops <- ask
            pure ((pc >= length ops) || not (o `isPrefixOf` original))
        )

search :: [Int] -> (Int, Int, Int, [Op]) -> Int
search original (a, b, c, ops) =
    let ((), (_, out, _, _, _), ()) = runRWS (runNoHalt original) ops (0, [], a, b, c)
     in if out == original
            then a
            else search original (a + 1, b, c, ops)

part2 :: IO ()
part2 = do
    input@(a, b, c, ops, original) <- parse parser <$> getInput (Main) 2024 17
    print input
    print original
    let a' = search original (0, b, c, ops)
    print a'
    pure ()
