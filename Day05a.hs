{-
--- Day 5: A Maze of Twisty Trampolines, All Alike ---

An urgent interrupt arrives from the CPU: it's trapped in a maze of jump instructions, and it would like assistance from any programs with spare cycles to help find the exit.

The message includes a list of the offsets for each jump. Jumps are relative: -1 moves to the previous instruction, and 2 skips the next one. Start at the first instruction in the list. The goal is to follow the jumps until one leads outside the list.

In addition, these instructions are a little strange; after each jump, the offset of that instruction increases by 1. So, if you come across an offset of 3, you would move three instructions forward, but change it to a 4 for the next time it is encountered.

For example, consider the following list of jump offsets:

0
3
0
1
-3
Positive jumps ("forward") move downward; negative jumps move upward. For legibility in this example, these offset values will be written all on one line, with the current instruction marked in parentheses. The following steps would be taken before an exit is found:

(0) 3  0  1  -3  - before we have taken any steps.
(1) 3  0  1  -3  - jump with offset 0 (that is, don't jump at all). Fortunately, the instruction is then incremented to 1.
 2 (3) 0  1  -3  - step forward because of the instruction we just modified. The first instruction is incremented again, now to 2.
 2  4  0  1 (-3) - jump all the way to the end; leave a 4 behind.
 2 (4) 0  1  -2  - go back to where we just were; increment -3 to -2.
 2  5  0  1  -2  - jump 4 steps forward, escaping the maze.
In this example, the exit is reached in 5 steps.

How many steps does it take to reach the exit?

Your puzzle answer was 336905.
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
        zipper' = ((l + 1):ls, rsl)

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
