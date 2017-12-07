{-
--- Day 3: Spiral Memory ---

You come across an experimental new kind of memory stored on an infinite two-dimensional grid.

Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...
While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.

For example:

Data from square 1 is carried 0 steps, since it's at the access port.
Data from square 12 is carried 3 steps, such as: down, left, left.
Data from square 23 is carried only 2 steps: up twice.
Data from square 1024 must be carried 31 steps.
How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?

Your puzzle input is 368078.
-}

import Data.Maybe (fromJust)
import Data.List (find)

calcSteps :: Int -> IO Int
calcSteps input = do
    putStrLn $ "input: " ++ show input
    print maxb
    putStrLn $ "mxs: " ++ show mxs ++ " esz: " ++ show esz
    putStrLn $ "levpo: " ++ show stp
    putStrLn $ "level: " ++ show stl
    putStrLn $ "froms: " ++ show frs
    putStrLn $ "xi: " ++ show xi
    putStrLn $ "yi: " ++ show yi
    putStrLn $ "up: " ++ show up
    putStrLn $ "lf: " ++ show lf
    putStrLn $ "dn: " ++ show dn
    putStrLn $ "rg: " ++ show rg
    putStrLn $ "x:  " ++ show x
    putStrLn $ "y:  " ++ show y
    return $ abs x + abs y
  where
    stv = snd maxl
    stp = fst maxl
    stl = (stp + 1) `div` 2
    frs = input - stv
    xi  = stl
    yi  = -stl
    esz = mxs `div` 4
    up  = min esz frs
    lf  = min esz (max 0 (frs - esz))
    dn  = min esz (max 0 (frs - 2 * esz))
    rg  = min esz (max 0 (frs - 3 * esz))
    x   = xi + rg - lf
    y   = yi + up - dn
    mxs = snd maxu - snd maxl
    dl  = (\v -> (v, v ^ 2)) <$> [1,3..]
    maxb = (\(r2, v) -> let r1 = r2 - 2 in ((r1, r1 ^ 2), (r2, v))) . fromJust $ find (\(_, v) -> input <= v) dl
    maxl = fst maxb
    maxu = snd maxb
    -- xd2 = 9 : 7 : replicate 200 8
    -- xd1 = reverse $ foldl (\l@(x:_) b -> x + b : l) [1] xd2
    -- xl  = reverse $ foldl (\l@(x:_) b -> x + b : l) [0] xd1
    -- maxb = (\(r2, v) -> let r1 = r2 - 2 in ((r1, r1 ^ 2), (r2, v))) . fromJust $ find (\(_, v) -> input <= v) c


test1 :: IO ()
test1 = print =<< calcSteps 55

main :: IO ()
main = print =<< calcSteps 368078


{-
65  64  63  62  61  60  59  58  57
66  37  36  35  34  33  32  31  56  89 130 179 236
67  38  17  16  15  14  13  30  55  88 129 178 235
68  39  18   5   4   3  12  29  54  87 128 177 234
69  40  19   6   1   2  11  28  53  86 127 176 233
70  41  20   7   8   9  10  27  52  85 126 175 232
71  42  21  22  23  24  25  26  51  84 125 174 231
72  43  44  45  46  47  48  49  50  83 124 173 230
73  74  75  76  77  78  79  80  81  82 123 172 229
                                   121 122 171 228
                                       169 170 227
                                           225 226
                                               289
121+(169-121)/8 = 127

8 16 24 32 40 48
 8  8  8  8  8

1 10 17 25 33 41 49 57
 9  7  8  8  8  8  8

1  4  15  34  61
 3  11   9  17

1  6  19  40  69
 5  13  21  29

-}