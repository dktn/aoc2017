{-
--- Part Two ---

For added security, yet another system policy has been put in place. Now, a valid passphrase must contain no two words that are anagrams of each other - that is, a passphrase is invalid if any word's letters can be rearranged to form any other word in the passphrase.

For example:

abcde fghij is a valid passphrase.
abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
iiii oiii ooii oooi oooo is valid.
oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.
Under this new system policy, how many passphrases are valid?

Although it hasn't changed, you can still get your puzzle input.
-}

import qualified Data.Set as Set
import Data.List

isValid :: [String] -> Bool
isValid input = Set.size set == length input
  where
    set = Set.fromList $ fmap sort input

countValid :: [[String]] -> Int
countValid = foldl (\a v -> if isValid v then a + 1 else a) 0

readInput :: String -> IO [[String]]
readInput file = do
    content <- readFile file
    return $ words <$> lines content

test1, test2, test3, test4, test5 :: IO ()
test1 = print $ countValid [words "abcde fghij"]
test2 = print $ countValid [words "abcde xyz ecdab"]
test3 = print $ countValid [words "a ab abc abd abf abj"]
test4 = print $ countValid [words "iiii oiii ooii oooi oooo"]
test5 = print $ countValid [words "oiii ioii iioi iiio"]

main :: IO ()
main = do
    input <- readInput "Day04.data"
    print $ countValid input
