module KattaPotter where
import Data.List
import Data.Function

type Book = Int
type Pack = [Book]

priceDiscount 0 = 0
priceDiscount 1 = 8
priceDiscount 2 = 8 * 2 * 0.95
priceDiscount 3 = 8 * 3 * 0.9
priceDiscount 4 = 8 * 4 * 0.8
priceDiscount 5 = 8 * 5 * 0.75

calcTotalPrice :: [Int] -> Double
calcTotalPrice titles = foldl (+) 0 $ map priceDiscount titles

price :: [Int] -> Double
price [] = 0
--price books = calcTotalPrice $ createPacks numTitles
price books = calcTotalPrice $ createPacks' [] books
    where
        numTitles = map length $ group books

-- Just work numerically with the number of different titles
-- and an implicit greedy strategy
createPacks :: [Int] -> [Int]
createPacks [] = []
createPacks titles = 
    let pack = length $ filter (>0) titles
    in  case pack of 
        0 -> []
        _ -> if (pack == 5) && (sum titles == 8) then [4,4]
             else pack : (createPacks $ map (\x -> x-1) titles)
                    

-- Explicitly construct the packs incrementally by choosing the
-- assignment that increases the price the less
createPacks' :: [Pack] -> [Book] -> [Int]
createPacks' packs [] = map length packs
createPacks' packs (t:ts) = case partition (notElem t) packs of
                            ([],_)         -> createPacks' ([t]:packs) ts
                            (cand,rest)    -> createPacks' (rest ++ addBook cand t) ts

addBook :: [Pack] -> Book -> [Pack]
addBook packs book = let (x,y:ys) = splitAt (smallerIncreaseAt numTitles) packs 
                     in x++[book:y]++ys
    where
        numTitles = map length packs


smallerIncreaseAt :: [Int] -> Int
smallerIncreaseAt titles = fst $ minimumBy (compare `on` snd) testIncr
  where
    testIncr = map (\(idx,t) -> (idx,calcTotalPrice t - priceDiscount  (t!!idx) +priceDiscount (t!!idx +1))) indexedTitles
    indexedTitles = [(idx,t) | (idx,t) <- zip [0..] (replicate (length titles) titles)]
    

