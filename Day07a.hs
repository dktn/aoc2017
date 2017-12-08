{-
--- Day 7: Recursive Circus ---

Wandering further through the circuits of the computer, you come upon a tower of programs that have gotten themselves into a bit of trouble. A recursive algorithm has gotten out of hand, and now they're balanced precariously in a large tower.

One program at the bottom supports the entire tower. It's holding a large disc, and on the disc are balanced several more sub-towers. At the bottom of these sub-towers, standing on the bottom disc, are other programs, each holding their own disc, and so on. At the very tops of these sub-sub-sub-...-towers, many programs stand simply keeping the disc below them balanced but with no disc of their own.

You offer to help, but first you need to understand the structure of these towers. You ask each program to yell out their name, their weight, and (if they're holding a disc) the names of the programs immediately above them balancing on that disc. You write this information down (your puzzle input). Unfortunately, in their panic, they don't do this in an orderly fashion; by the time you're done, you're not sure which program gave which information.

For example, if your list is the following:

pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
...then you would be able to recreate the structure of the towers that looks like this:

                gyxo
              /
         ugml - ebii
       /      \
      |         jptl
      |
      |         pbga
     /        /
tknk --- padx - havc
     \        \
      |         qoyq
      |
      |         ktlj
       \      /
         fwft - cntj
              \
                xhth
In this example, tknk is at the bottom of the tower (the bottom program), and is holding up ugml, padx, and fwft. Those programs are, in turn, holding up other programs; in this example, none of those programs are holding up any other programs, and are all the tops of their own towers. (The actual tower balancing in front of you is much larger.)

Before you're ready to help them, you need to make sure your information is correct. What is the name of the bottom program?
-}

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)
import Data.List.Split as Split
import Data.List
import Data.Map.Strict as M hiding (foldl, foldr, split, filter)
import Text.Show.Pretty (ppShow)

data Tree = Leaf | Children [String]
            deriving (Show)

data Entry = Entry String Tree
             deriving (Show)

data EntryCh = EntryCh String (Maybe Bool) deriving (Show)

dos :: String -> IO ()
dos = undefined

toMap :: [Entry] -> Map String Entry
toMap = foldl insertToMap M.empty
  where
    insertToMap emap elem@(Entry name _) = M.insert name elem emap

toFlatList :: [Entry] -> [EntryCh]
toFlatList = foldr insertToFlatList []
  where
    insertToFlatList elem@(Entry name (Children children)) elist = EntryCh name (Just True)  : childrenE ++ elist
      where
        childrenE = (`EntryCh` Nothing) <$> children
    insertToFlatList elem@(Entry name _                  ) elist = EntryCh name (Just False) : elist

toEntry :: String -> Entry
toEntry input = case parse parseExpr "" input of
    Right entry -> entry
    Left  err   -> undefined

parseExpr :: Parser Entry
parseExpr = try parseParent
            <|> parseSingle

parseSingle :: Parser Entry
parseSingle = do
    name <- many (noneOf " ")
    spaces
    weight <- many (noneOf " ")
    return $ Entry name Leaf

parseParent :: Parser Entry
parseParent = do
    name <- many letter
    spaces
    weight <- many (noneOf " ")
    spaces
    char '-'
    char '>'
    spaces
    children <- many (noneOf "\n")
    return $ Entry name $ Children $ split (dropBlanks . dropDelims $ Split.oneOf ", ") children

readInput :: String -> IO [Entry]
readInput file = do
    content <- readFile file
    return $ toEntry <$> lines content

pShow :: Show a => String -> a -> IO ()
pShow label obj = do
    putStrLn $ label ++ ":"
    putStrLn $ ppShow obj
    return ()

loadInput :: IO [Entry]
loadInput = readInput "Day07.data"

entryEq :: EntryCh -> EntryCh -> Bool
entryEq (EntryCh a _) (EntryCh b _) = a == b

entryOrd :: EntryCh -> EntryCh -> Ordering
entryOrd (EntryCh a _) (EntryCh b _) = compare a b

main :: IO ()
main = do
    input <- loadInput
    -- pShow "input" input
    -- let inputMap = toMap input
    -- pShow "inputMap"
    let inputList = toFlatList input
    -- pShow "inputList" inputList
    let grInput = groupBy entryEq $ sortBy entryOrd inputList
    -- pShow "grInput" grInput
    pShow "filtered" $ filter (\e -> length e == 1) grInput
    -- dos "ngrmq (80)"
    -- dos "ngrmq (80) -> cluej, ywrxbgi, saznyj"
