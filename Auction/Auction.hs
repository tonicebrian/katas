
import System.Environment

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

main = do
  args <- getArgs
  contents <- readFile (args !! 0)
  let problems = map (map (read :: String->Int) . words) $ tail $ lines contents
  putStrLn "Hello world!!"
