
data Product = P Double Double -- Price & Weight

instance Eq Product where
  (P p1 w1) == (P p2 w2) = (p1 == p2) && (w1 == w2)

instance Ord Product where
  compare (P p1 w1) (P p2 w2) 
     | ((p1 < p2) && (w1 <= w2)) || ((p1 <= p2) && (w1 < w2)) = GT 
     | otherwise  = LT                             

isBargain :: Product -> [Product] -> Bool
isBargain p ps = p == (maximum ps)
