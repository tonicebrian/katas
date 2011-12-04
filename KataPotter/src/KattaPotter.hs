module KattaPotter where

price :: [Int] -> Double
price [] = 0
price _ = -1

priceDiscount 1 = 8
priceDiscount 2 = 8 * 2 * 0.95
priceDiscount 3 = 8 * 3 * 0.9
priceDiscount 4 = 8 * 4 * 0.8
priceDiscount 5 = 8 * 5 * 0.75
