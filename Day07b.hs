{-
--- Part Two ---

The programs explain the situation: they can't get down. Rather, they could get down, if they weren't expending all of their energy trying to keep the tower balanced. Apparently, one program has the wrong weight, and until it's fixed, they're stuck here.

For any program holding a disc, each program standing on that disc forms a sub-tower. Each of those sub-towers are supposed to be the same weight, or the disc itself isn't balanced. The weight of a tower is the sum of the weights of the programs in that tower.

In the example above, this means that for ugml's disc to be balanced, gyxo, ebii, and jptl must all have the same weight, and they do: 61.

However, for tknk to be balanced, each of the programs standing on its disc and all programs above it must each match. This means that the following sums must all be the same:

ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243
As you can see, tknk's disc is unbalanced: ugml's stack is heavier than the other two. Even though the nodes above ugml are balanced, ugml itself is too heavy: it needs to be 8 units lighter for its stack to weigh 243 and keep the towers balanced. If this change were made, its weight would be 60.

Given that exactly one program is the wrong weight, what would its weight need to be to balance the entire tower?

Although it hasn't changed, you can still get your puzzle input.

362

370 - 8

Please forgive the style, made in rush
-}

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)
import Data.List.Split as Split
import Data.List
import Data.Maybe
import Data.Map.Strict as M hiding (foldl, foldr, split, filter)
import Text.Show.Pretty (ppShow)

data SimpleTree = SimpleLeaf | SimpleChildren [String]
            deriving (Show)

data Entry = Entry String Int SimpleTree
             deriving (Show)

data EntryCh = EntryCh String (Maybe Bool) deriving (Show)

data Tree = Leaf String Int | Children String Int [Tree]
            deriving (Show)

dos :: String -> IO ()
dos = undefined

toMap :: [Entry] -> Map String Entry
toMap = foldl insertToMap M.empty
  where
    insertToMap emap elem@(Entry name _ _) = M.insert name elem emap

toFlatList :: [Entry] -> [EntryCh]
toFlatList = foldr insertToFlatList []
  where
    insertToFlatList elem@(Entry name w (SimpleChildren children)) elist = EntryCh name (Just True)  : childrenE ++ elist
      where
        childrenE = (`EntryCh` Nothing) <$> children
    insertToFlatList elem@(Entry name w _                  ) elist = EntryCh name (Just False) : elist

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
    return $ Entry name (read weight) SimpleLeaf

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
    return $ Entry name (read weight) $ SimpleChildren $ split (dropBlanks . dropDelims $ Split.oneOf ", ") children

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

buildTree :: Map String Entry -> String -> Tree
buildTree emap root = case M.lookup root emap of
    Nothing -> error "nothing"
    Just e  -> case e of
        Entry name w (SimpleChildren l) -> Children name w $ buildTree emap <$> l
        Entry name w _                  -> Leaf name w

getWeight :: Tree -> Int
getWeight (Children _ w _) = w
getWeight (Leaf _ w) = w

calcTree :: Tree -> Tree
calcTree (Children name w children) = Children name sumW newChildren
  where
    newChildren = calcTree <$> children
    sumW = w + foldl (\a ch -> a + getWeight ch) 0 newChildren
calcTree l@(Leaf name w) = l

findUnbalanced :: Tree -> Maybe Tree
findUnbalanced b@(Children name w children) = result
  where
    result = if w == sumCh
        then find (isJust . findUnbalanced) children
        else Just b
    sumCh = foldl (\a v -> a + getWeight v) 0 children
findUnbalanced l = Nothing

cutGrandChi :: Int -> Tree -> Tree
cutGrandChi 0 t = Leaf "end" 0
cutGrandChi l (Children name w ch) = Children name w $ cutGrandChi (l - 1) <$> ch
cutGrandChi l t = t

findByName :: String -> Tree -> Maybe Tree
findByName n t@(Children name w ch)
  | name == n = Just t
  | otherwise = find (isJust . findByName n) ch
findByName n t@(Leaf name w) = if name == n then Just t else Nothing

main :: IO ()
main = do
    input <- loadInput
    let inputList = toFlatList input
    -- pShow "inputList" inputList
    let grInput = groupBy entryEq $ sortBy entryOrd inputList
    -- pShow "grInput" grInput
    -- pShow "filtered" $ filter (\e -> length e == 1) grInput
    let root = (\(EntryCh n _) -> n) . head . head $ filter (\e -> length e == 1) grInput
    pShow "result" root

    let inputMap = toMap input
    -- pShow "inputMap" inputMap
    let tree = buildTree inputMap root
    -- pShow "tree" tree
    let ctree = calcTree tree
    -- pShow "ctree" ctree
    let unba = fromJust $ findUnbalanced ctree
    -- pShow "unba" unba
    let cunba = cutGrandChi 3 unba
    -- pShow "cunba" cunba
    let fbn = fromJust $ findByName "nbyij" ctree
    pShow "fbn" fbn

{-
67025 nbyil (32760)
  11427 tdxow (7867)
    1184 htzrkz (99)
    1184 mrxqlw (566)
    1192 ghwgd (370)
  11419
  11419

-}