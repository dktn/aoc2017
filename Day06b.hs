{-
--- Part Two ---

Out of curiosity, the debugger would also like to know the size of the loop: starting from a state that has already been seen, how many block redistribution cycles must be performed before that same state is seen again?

In the example above, 2 4 1 2 is seen again after four cycles, and so the answer in that example would be 4.

How many cycles are in the infinite loop that arises from the configuration in your puzzle input?

Your puzzle answer was 2793.

Both parts of this puzzle are complete! They provide two gold stars: **
-}

import Data.List
import Data.Maybe (fromJust)
import Data.Map.Strict as M

zero :: Int -> [Int] -> [Int]
zero idx banks = case splitAt idx banks of
    (l, e:r) -> l ++ 0 : r
    _ -> banks

distribute :: Int -> Int -> [Int] -> ([Int], Int)
distribute 0     _   banks = (banks, 0)
distribute count idx banks = distribute (count - 1) nextIdx banks'
  where
    nextIdx = (idx + 1) `mod` length banks -- not effective
    banks' = case splitAt idx banks of
        (l, e:r) -> l ++ (e + 1) : r
        _ -> banks

redistribute :: Int -> Int -> [Int] -> [Int]
redistribute count idx = fst . distribute count idx

loop :: Int -> Map [Int] Int -> [Int] -> Int
loop step smap banks = result
  where
    maxVal = maximum banks
    maxPos = fromJust $ elemIndex maxVal banks
    banks0 = zero maxPos banks
    banks' = redistribute maxVal ((maxPos + 1) `mod` length banks) banks0
    smap'   = M.insert banks' step smap
    result = if M.member banks' smap
        then let step0 = fromJust $ M.lookup banks' smap in step - step0
        else loop (step + 1) smap' banks'

countSteps :: [Int] -> Int
countSteps = loop 1 M.empty

test1 :: IO ()
test1 = print $ countSteps [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]

test2 :: IO ()
test2 = print $ countSteps [0, 2, 7, 0]

main :: IO ()
main = test1
