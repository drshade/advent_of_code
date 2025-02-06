module Day17 where
import           Data.Functor (($>))
import           Handy
import           Text.Parsec  (anyChar, char, choice, many1)

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

parser :: Parser [Op]
parser =
    many1 op
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
            literal = read <$> many1 anyChar
            combo :: Parser Combo
            combo = do
                v <- literal
                pure $ if v <= 3 then Lit v else case v of
                    4 -> RegA
                    5 -> RegB
                    6 -> RegC
                    _ -> error "spec lied!"

part1 :: IO ()
part1 = do
    input <- parse parser <$> getInput Main 2024 17
    putStrLn $ show input
