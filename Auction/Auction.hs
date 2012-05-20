
import System.Environment
import Data.List
import Data.Function

data Product = P Double Double -- Price & Weight

instance Eq Product where
  (P p1 w1) == (P p2 w2) = (p1 == p2) && (w1 == w2)

instance Ord Product where
  compare (P p1 w1) (P p2 w2) 
     | ((p1 < p2) && (w1 <= w2)) || ((p1 <= p2) && (w1 < w2)) = GT 
     | otherwise  = LT

isBargain :: Product -> [Product] -> Bool
isBargain p ps = p == (maximum ps)
  
isTerrible :: Product -> [Product] -> Bool
isTerrible p ps = p == (minimum ps)  

findLowest points = loop points []
  where
    loop (a:[]) ps = a:ps
    loop (a@(x1,y1):b@(x2,y2):rest) ps =        
      if (y1 < y2) || (y1 == y2 && x1 < x2)
      then loop (a:rest) (b:ps)
      else loop (b:rest) (a:ps)

turn :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Turn
turn (ax,ay) (bx,by) (cx,cy) = case compare cross 0 of
               GT -> Main.Left
               EQ -> Main.Colinear
               LT -> Main.Right
    where
      cross = x1 * y2 - x2 * y1
      x1 = bx - ax
      y1 = by - ay
      x2 = cx - bx
      y2 = cy - by

cosine (x1,y1) (x2,y2) = dx / len
  where
    dx = fromIntegral $ x1 - x2
    dy = fromIntegral $ y1 - y2
    len = sqrt (dx * dx + dy * dy)

data Turn = Left | Right | Colinear
            deriving (Show, Eq)

grahamScan :: [(Int,Int)] -> [(Int,Int)]
grahamScan points
    | length points < 3 = error "Degenerate"
    | otherwise         = 
        let (firstPoint:rest) = findLowest points
            sortedRest = sortBy (compare `on` (cosine firstPoint)) rest

            loop (a:b:[]) = case turn a b firstPoint of
                              Main.Left -> b : []
                              _    -> []
            loop (a:b:c:ps) = case turn a b c of
                                Main.Left -> b : loop (b:c:ps)
                                _    -> loop (a:c:ps)

        in firstPoint : loop (firstPoint:sortedRest)

main = do
  args <- getArgs
  contents <- readFile (args !! 0)
  let problems = map (map (read :: String->Int) . words) $ tail $ lines contents
  putStrLn "Hello world!!"
