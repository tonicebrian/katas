module KattaPotter where
import Data.List

price :: [Int] -> Double
price [] = 0
price books = foldl (+) 0 $ map priceDiscount $ createPack numTitles
    where
        numTitles = map length $ group books

createPack :: [Int] -> [Int]
createPack [] = []
createPack titles = 
    let pack = length $ filter (>0) titles
    in  case pack of 
        0 -> []
        _ -> if (pack == 5) && (sum titles == 8) then [4,4]
             else pack : (createPack $ map (\x -> x-1) titles)
                    

priceDiscount 0 = 0
priceDiscount 1 = 8
priceDiscount 2 = 8 * 2 * 0.95
priceDiscount 3 = 8 * 3 * 0.9
priceDiscount 4 = 8 * 4 * 0.8
priceDiscount 5 = 8 * 5 * 0.75
