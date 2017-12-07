{-
--- Day 4: High-Entropy Passphrases ---

A new system policy has been put in place that requires all accounts to use a passphrase instead of simply a password. A passphrase consists of a series of words (lowercase letters) separated by spaces.

To ensure security, a valid passphrase must contain no duplicate words.

For example:

aa bb cc dd ee is valid.
aa bb cc dd aa is not valid - the word aa appears more than once.
aa bb cc dd aaa is valid - aa and aaa count as different words.
The system's full passphrase list is available as your puzzle input. How many passphrases are valid?

To begin, get your puzzle input.

Your puzzle answer was 477.

The first half of this puzzle is complete! It provides one gold star: *
-}

import qualified Data.Set as Set

isValid :: [String] -> Bool
isValid input = Set.size set == length input
  where
    set = Set.fromList input

countValid :: [[String]] -> Int
countValid = foldl (\a v -> if isValid v then a + 1 else a) 0

readInput :: String -> IO [[String]]
readInput file = do
    content <- readFile file
    return $ words <$> lines content

test1 :: IO ()
test1 = print $ countValid [words "aa bb cc dd ee"]

test2 :: IO ()
test2 = print $ countValid [words "aa bb cc dd ee", words "aa bb cc dd aa"]

main :: IO ()
main = do
    input <- readInput "Day04.data"
    print $ countValid input
