{-
--- Part Two ---

Now, you're ready to remove the garbage.

To prove you've removed it, you need to count all of the characters within the garbage. The leading and trailing < and > don't count, nor do any canceled characters or the ! doing the canceling.

<>, 0 characters.
<random characters>, 17 characters.
<<<<>, 3 characters.
<{!>}>, 2 characters.
<!!>, 0 characters.
<!!!>>, 0 characters.
<{o"i!a,<{i<a>, 10 characters.
How many non-canceled characters are within the garbage in your puzzle input?


-}

import Control.Monad (void)
import Data.List
import Data.Maybe


type Score = Int
type Level = Int
type Omit = Bool
type Garbage = Bool
type GarbageChars = Int

type ScoreState = (Score, (Level, Omit, Garbage, GarbageChars))

loadInput :: IO String
loadInput = readFile "Day09.data"

test1 :: String
test1 = "<{o\"i!a,<{i<a>"

score :: String -> ScoreState
score = foldl calc (0, (0, False, False, 0))

calc :: ScoreState -> Char -> ScoreState
calc (s, (l, True, g,     c)) _   = (s,     (l, False, g,     c    ))
calc (s, (l, o,    g,     c)) '!' = (s,     (l, True,  g,     c    ))
calc (s, (l, o,    False, c)) '<' = (s,     (l,     o, True,  c    ))
calc (s, (l, o,    True , c)) '>' = (s,     (l,     o, False, c    ))
calc (s, (l, o,    False, c)) '}' = (s + l, (l - 1, o, False, c    ))
calc (s, (l, o,    False, c)) '{' = (s,     (l + 1, o, False, c    ))
calc (s, (l, o,    True , c)) ch  = (s,     (l,     o, True,  c + 1))
calc (s, (l, o,    g,     c)) ch  = (s,     (l,     o, g,     c    ))

runTwo :: IO ()
runTwo = do
    input <- loadInput
    print $ score input

main :: IO ()
main = runTwo