{-
--- Day 10: Knot Hash ---

You come across some programs that are trying to implement a software emulation of a hash based on knot-tying. The hash these programs are implementing isn't very strong, but you decide to help them anyway. You make a mental note to remind the Elves later not to invent their own cryptographic functions.

This hash function simulates tying a knot in a circle of string with 256 marks on it. Based on the input to be hashed, the function repeatedly selects a span of string, brings the ends together, and gives the span a half-twist to reverse the order of the marks within it. After doing this many times, the order of the marks is used to build the resulting hash.

  4--5   pinch   4  5           4   1
 /    \  5,0,1  / \/ \  twist  / \ / \
3      0  -->  3      0  -->  3   X   0
 \    /         \ /\ /         \ / \ /
  2--1           2  1           2   5
To achieve this, begin with a list of numbers from 0 to 255, a current position which begins at 0 (the first element in the list), a skip size (which starts at 0), and a sequence of lengths (your puzzle input). Then, for each length:

Reverse the order of that length of elements in the list, starting with the element at the current position.
Move the current position forward by that length plus the skip size.
Increase the skip size by one.
The list is circular; if the current position and the length try to reverse elements beyond the end of the list, the operation reverses using as many extra elements as it needs from the front of the list. If the current position moves past the end of the list, it wraps around to the front. Lengths larger than the size of the list are invalid.

Here's an example using a smaller list:

Suppose we instead only had a circular list containing five elements, 0, 1, 2, 3, 4, and were given input lengths of 3, 4, 1, 5.

The list begins as [0] 1 2 3 4 (where square brackets indicate the current position).
The first length, 3, selects ([0] 1 2) 3 4 (where parentheses indicate the sublist to be reversed).
After reversing that section (0 1 2 into 2 1 0), we get ([2] 1 0) 3 4.
Then, the current position moves forward by the length, 3, plus the skip size, 0: 2 1 0 [3] 4. Finally, the skip size increases to 1.
The second length, 4, selects a section which wraps: 2 1) 0 ([3] 4.
The sublist 3 4 2 1 is reversed to form 1 2 4 3: 4 3) 0 ([1] 2.
The current position moves forward by the length plus the skip size, a total of 5, causing it not to move because it wraps around: 4 3 0 [1] 2. The skip size increases to 2.
The third length, 1, selects a sublist of a single element, and so reversing it has no effect.
The current position moves forward by the length (1) plus the skip size (2): 4 [3] 0 1 2. The skip size increases to 3.
The fourth length, 5, selects every element starting with the second: 4) ([3] 0 1 2. Reversing this sublist (3 0 1 2 4 into 4 2 1 0 3) produces: 3) ([4] 2 1 0.
Finally, the current position moves forward by 8: 3 4 2 1 [0]. The skip size increases to 4.
In this example, the first two numbers in the list end up being 3 and 4; to check the process, you can multiply them together to produce 12.

However, you should instead use the standard list size of 256 (with values 0 to 255) and the sequence of lengths in your puzzle input. Once this process is complete, what is the result of multiplying the first two numbers in the list?

Your puzzle answer was 23715.

The first half of this puzzle is complete! It provides one gold star: *

-}

import Control.Monad (void)
import Data.List
import Data.Maybe

data State = State { _size :: Int, _pos :: Int, _skip :: Int, _list :: [Int] } deriving Show

input :: (Int, [Int])
input = (256, [94, 84, 0, 79, 2, 27, 81, 1, 123, 93, 218, 23, 103, 255, 254, 243])

inputTest :: (Int, [Int])
inputTest = (5, [3, 4, 1, 5])

calc :: Int -> [Int] -> Int
calc size lengths = a * b
  where
    (a:b:_) = calcList size lengths

calcB :: Int -> [Int] -> ([Int], State)
calcB size lengths = (calcList size lengths, calcState size lengths)

calcList :: Int -> [Int] -> [Int]
calcList size lengths = fwd (size - _pos state) (_list state)
  where
    state = calcState size lengths

calcState :: Int -> [Int] -> State
calcState size = process (State size 0 0 list)
  where
    list = [0 .. (size - 1)]

process :: State -> [Int] -> State
process = foldl step

step :: State -> Int -> State
step (State size pos skip list) len = state'
  where
    list'  = rev len list
    list'' = fwd steps list'
    steps  = (len + skip) `mod` size
    pos'   = (pos + steps) `mod` size
    state' = State size pos' (skip + 1) list''

rev :: Int -> [Int] -> [Int]
rev len list = reverse listL ++ listR
  where
    (listL, listR) = splitAt len list

fwd :: Int -> [Int] -> [Int]
fwd len list = take (length list) . drop len $ list ++ list

{-
-- 30124 pos 1 skip 3 [5]
-- 30124

-- 01234 p0
-- s0
-- 21034 p0 (s3,r)
-- 34210 p3 (f3+0) (b3 21034)
-- s1
-- 12430 p3 (s4,r)
-- 12430 p3 (f4+1) (b3 43012)
-- s2
-- 12430 p3 (s1,r)
-- 30124 p1 (f1+2) (b1 43012)
-- s3
-- 42103 p1 (s5,r)
-- 03421 p4 (f5+3) (b4 34210)


-> [0] 1 2 3 4
-- s0
s l3    -> ([0] 1 2) 3 4
r l3    -> ([2] 1 0) 3 4
f l3+s0 ->   2  1 0 [3] 4
-- s1
s l4    -> 2 1) 0 ([3] 4
r l4    -> 4 3) 0 ([1] 2
f l4+s1 -> 4 3  0  [1] 2
-- s2
s l1    -> 4  3  0 ([1]) 2
r
f l1+s2 -> 4 [3] 0   1   2
-- s3
s l5    -> 4) ([3] 0 1  2
r l5    -> 3) ([4] 2 1  0)
f l5+s3 -> 3    4  2 1 [0]
-- s4
-}

runOne :: IO ()
runOne = print $ uncurry calc input

main :: IO ()
main = runOne
