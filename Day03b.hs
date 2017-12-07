{-
--- Part Two ---

As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.

So, the first few squares' values are chosen as follows:

Square 1 starts with the value 1.
Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...
What is the first value written that is larger than your puzzle input?

Your puzzle input is 368078.
-}

import Control.Monad (liftM2, liftM, forM_)
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Matrix (Matrix)
import qualified Data.Matrix as M

size = 300
delt = size `div` 2

getEl :: (Int, Int) -> Matrix Int -> Int
getEl (x, y) = M.getElem (-y + delt) (x + delt)

setEl :: Int -> (Int, Int) -> Matrix Int -> Matrix Int
setEl a (x, y) = M.setElem a (-y + delt, x + delt)

calcSteps :: Int -> IO (Maybe Int)
calcSteps input = do
    putStrLn $ "input: " ++ show input
    -- print m
    return v
  where
    zm = setEl 1 (0, 0) $ M.zero size size
    l  = [2..80] :: [Int]
    lp = calcXY <$> l
    (m, v) = foldl sv (zm, Nothing) lp
    sv (n, Just v)  p = (n, Just v)
    sv (n, Nothing) p =
      if val < input
      then (m, Nothing)
      else (m, Just val)
      where
        m = setEl val p n
        val = getEl (rr p) n
            + getEl (ru p) n
            + getEl (uu p) n
            + getEl (lu p) n
            + getEl (ll p) n
            + getEl (ld p) n
            + getEl (dd p) n
            + getEl (rd p) n

rr, ru, uu, lu, ll, ld, dd, rd :: (Int, Int) -> (Int, Int)
rr (x, y) = (x + 1, y    )
ru (x, y) = (x + 1, y + 1)
uu (x, y) = (x    , y + 1)
lu (x, y) = (x - 1, y + 1)
ll (x, y) = (x - 1, y   )
ld (x, y) = (x - 1, y - 1)
dd (x, y) = (x    , y - 1)
rd (x, y) = (x + 1, y - 1)

calcXY :: Int -> (Int, Int)
calcXY input = (x, y)
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

test1 :: IO ()
test1 = print =<< calcSteps 55

main :: IO ()
main = print =<< calcSteps 368078
