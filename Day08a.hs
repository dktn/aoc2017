{-
--- Day 8: I Heard You Like Registers ---

You receive a signal directly from the CPU. Because of your recent assistance with jump instructions, it would like you to compute the result of a series of unusual register instructions.

Each instruction consists of several parts: the register to modify, whether to increase or decrease that register's value, the amount by which to increase or decrease it, and a condition. If the condition fails, skip the instruction without modifying the register. The registers all start at 0. The instructions look like this:

b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
These instructions would be processed as follows:

Because a starts at 0, it is not greater than 1, and so b is not modified.
a is increased by 1 (to 1) because b is less than 5 (it is 0).
c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
c is increased by -20 (to -10) because c is equal to 10.
After this process, the largest value in any register is 1.

You might also encounter <= (less than or equal to) or != (not equal to). However, the CPU doesn't have the bandwidth to tell you what all the registers are named, and leaves that to you to determine.

What is the largest value in any register after completing the instructions in your puzzle input?

Your puzzle answer was 4163.

The first half of this puzzle is complete! It provides one gold star: *
-}

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)
import Data.List
import Data.Maybe
import Data.Map.Strict as M hiding (foldl, foldr, split, filter)
import Text.Show.Pretty (ppShow)

type Register = String
type Value = Int

data CondOp = LR | LER | GR | GER | ER | NER deriving Show

data ActionOp = Dec | Inc deriving Show

data Cond = Cond Register CondOp Value deriving Show

data Action = Action Register ActionOp Value deriving Show

data Instr = Instr Action Cond deriving Show

calcMax :: [Instr] -> Value
calcMax instrl = result
  where
    mp = foldl eval M.empty instrl
    result = maximum $ M.elems mp

getMap :: Register -> Map Register Value -> Value
getMap r = fromMaybe 0 . M.lookup r

evalCondOp :: Ord a => CondOp -> a -> a -> Bool
evalCondOp LR  a b = a <  b
evalCondOp LER a b = a <= b
evalCondOp GR  a b = a >  b
evalCondOp GER a b = a >= b
evalCondOp ER  a b = a == b
evalCondOp NER a b = a /= b

evalAction :: ActionOp -> Int -> Int -> Int
evalAction Inc a b = a + b
evalAction Dec a b = a - b

eval :: Map Register Value -> Instr -> Map Register Value
eval mp (Instr (Action reg op val) (Cond regC opC valC)) =
    if evalCondOp opC (getMap regC mp) valC
        then let newVal = evalAction op (getMap reg mp) val in M.insert reg newVal mp
        else mp

toCond :: String -> CondOp
toCond "<"  = LR
toCond "<=" = LER
toCond ">"  = GR
toCond ">=" = GER
toCond "==" = ER
toCond "!=" = NER
toCond t = error $ "Can't parse " ++ t

toAction :: String -> ActionOp
toAction "inc" = Inc
toAction "dec" = Dec
toAction t = error $ "Can't parse " ++ t

toInstr :: String -> Instr
toInstr input = case parse parseInstr "" input of
    Right instr -> instr
    Left  err   -> error $ "ErrorT " ++ show err

parseInstr :: Parser Instr
parseInstr = do
    register <- many letter
    spaces
    action <- many letter
    spaces
    steps <- many (noneOf " ")
    spaces
    string "if"
    spaces
    registerC <- many letter
    spaces
    cond <- many (oneOf "<>=!")
    spaces
    val <- many (noneOf " ")
    return $ Instr (Action register (toAction action) (read steps)) (Cond registerC (toCond cond) (read val))

readInput :: String -> IO [Instr]
readInput file = do
    content <- readFile file
    return $ toInstr <$> lines content

pShow :: Show a => String -> a -> IO ()
pShow label obj = do
    putStrLn $ label ++ ":"
    putStrLn $ ppShow obj
    return ()

loadInput :: IO [Instr]
loadInput = readInput "Day08.data"

loadTestInput :: IO [Instr]
loadTestInput = readInput "Day08t.data"

main :: IO ()
main = do
    input <- loadInput
    pShow "" input
    print $ calcMax input
