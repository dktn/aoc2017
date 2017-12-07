{-
--- Day 6: Memory Reallocation ---

A debugger program here is having an issue: it is trying to repair a memory reallocation routine, but it keeps getting stuck in an infinite loop.

In this area, there are sixteen memory; each memory bank can hold any number of blocks. The goal of the reallocation routine is to balance the blocks between the memory banks.

The reallocation routine operates in cycles. In each cycle, it finds the memory bank with the most blocks (ties won by the lowest-numbered memory bank) and redistributes those blocks among the banks. To do this, it removes all of the blocks from the selected bank, then moves to the next (by index) memory bank and inserts one of the blocks. It continues doing this until it runs out of blocks; if it reaches the last memory bank, it wraps around to the first one.

The debugger would like to know how many redistributions can be done before a blocks-in-banks configuration is produced that has been seen before.

For example, imagine a scenario with only four memory banks:

The banks start with 0, 2, 7, and 0 blocks. The third bank has the most blocks, so it is chosen for redistribution.
Starting with the next bank (the fourth bank) and then continuing to the first bank, the second bank, and so on, the 7 blocks are spread out over the memory banks. The fourth, first, and second banks get two blocks each, and the third bank gets one back. The final result looks like this: 2 4 1 2.
Next, the second bank is chosen because it contains the most blocks (four). Because there are four memory banks, each gets one block. The result is: 3 1 2 3.
Now, there is a tie between the first and fourth memory banks, both of which have three blocks. The first bank wins the tie, and its three blocks are distributed evenly over the other three banks, leaving it with none: 0 2 3 4.
The fourth bank is chosen, and its four blocks are distributed such that each of the four banks receives one: 1 3 4 1.
The third bank is chosen, and the same thing happens: 2 4 1 2.
At this point, we've reached a state we've seen before: 2 4 1 2 was already seen. The infinite loop is detected after the fifth block redistribution cycle, and so the answer in this example is 5.

Given the initial block counts in your puzzle input, how many redistribution cycles must be completed before a configuration is produced that has been seen before?

Your puzzle answer was 4074.

The first half of this puzzle is complete! It provides one gold star: *
-}

import Data.List
import Data.Maybe (fromJust)
import Data.Set as S

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

loop :: Int -> Set [Int] -> [Int] -> Int
loop step set banks = result
  where
    maxVal = maximum banks
    maxPos = fromJust $ elemIndex maxVal banks
    banks0 = zero maxPos banks
    banks' = redistribute maxVal ((maxPos + 1) `mod` length banks) banks0
    set'   = S.insert banks' set
    result = if S.member banks' set
        then step
        else loop (step + 1) set' banks'

countSteps :: [Int] -> Int
countSteps = loop 1 S.empty

test1 :: IO ()
test1 = print $ countSteps [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]

test2 :: IO ()
test2 = print $ countSteps [0, 2, 7, 0]

main :: IO ()
main = test1
