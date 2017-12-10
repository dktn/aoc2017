{-
--- Part Two ---

To be safe, the CPU also needs to know the highest value held in any register during this process so that it can decide how much memory to allocate to these operations. For example, in the above instructions, the highest value ever held was 10 (in register c after the third instruction was evaluated).
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
    (mp, maxV) = foldl eval (M.empty, 0) instrl
    result = maxV

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

eval :: (Map Register Value, Int) -> Instr -> (Map Register Value, Int)
eval (mp, maxC) (Instr (Action reg op val) (Cond regC opC valC)) =
    if evalCondOp opC (getMap regC mp) valC
        then let newVal = evalAction op (getMap reg mp) val in (M.insert reg newVal mp, max maxC newVal)
        else (mp, maxC)

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
