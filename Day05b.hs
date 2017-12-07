{-
--- Part Two ---

Now, the jumps are even stranger: after each jump, if the offset was three or more, instead decrease it by 1. Otherwise, increase it by 1 as before.

Using this rule with the above example, the process now takes 10 steps, and the offset values after finding the exit are left as 2 3 2 3 -1.

How many steps does it now take to reach the exit?

Your puzzle answer was 21985262.

Both parts of this puzzle are complete! They provide two gold stars: **
-}


type Zipper a = ([a], [a])

mkZipper :: [a] -> Zipper a
mkZipper l = (l, [])

go :: Num a => Int -> Zipper a -> Maybe (Zipper a)
go offset (lsl@(l:ls), rsl)
    | offset >  0 = if steps <  length lsl then Just $ goForward  steps zipper' else Nothing
    | offset <  0 = if steps <= length rsl then Just $ goBackward steps zipper' else Nothing
    | offset == 0 = Just zipper'
      where
        steps = abs offset
        zipper' = (newL:ls, rsl)
        newL = if offset >= 3 then l - 1 else l + 1

goForward :: Int -> Zipper a -> Zipper a
goForward 0     zipper      = zipper
goForward steps (l:ls, rsl) = goForward (steps - 1) (ls, l:rsl)

goBackward :: Int -> Zipper a -> Zipper a
goBackward 0     zipper      = zipper
goBackward steps (lsl, r:rs) = goBackward (steps - 1) (r:lsl, rs)

traverseMaze :: Int -> Zipper Int -> Int
traverseMaze step zipper@(l:_, _) = case go l zipper of
    Just zipper' -> traverseMaze (step + 1) zipper'
    Nothing      -> step

countSteps :: [Int] -> Int
countSteps maze = traverseMaze 1 $ mkZipper maze

readInput :: String -> IO [Int]
readInput file = do
    content <- readFile file
    return $ read <$> lines content

test1 :: IO ()
test1 = print $ countSteps [0, 3, 0, 1, -3]

test2 :: IO ()
test2 = print $ countSteps [0, 7, 0, 1, -3]

main :: IO ()
main = do
    input <- readInput "Day05.data"
    print $ countSteps input
