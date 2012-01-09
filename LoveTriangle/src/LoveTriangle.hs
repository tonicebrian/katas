module LoveTriangle where

lovetriangle = solve

-- Directions for partial triangles
data Direction = Up | Down  | Straight | Done deriving Show

-- basic tools to manipulate base 2 expansions
div2Ok x = div x 2
mod2Ok x = mod x 2
equalZero x = 0 == x

ones =  foldl1 (+) 

makeZerosOnes 0 0 = []
makeZerosOnes 0 n = 1 : (makeZerosOnes 0 (n-1))
makeZerosOnes n m = 0 : (makeZerosOnes (n-1) m)

--
allZeros l = foldl1 (&&) (map equalZero l)


-- matching bit strings
-- first list has ones when second has and they are of the same length 
matches [] [] = True
matches _ [] = False
matches [] _ = False
matches (0:xs) (1:ys) = False
matches (_:xs) (_:ys) = matches xs ys 

-- lists manipulation
-- obtain next bitmap matrix generator
nextList l = map div2Ok l
-- obtain next bitmap column
nextColumn l = map mod2Ok l

-- a partial triangle is an tuple: area so far, 2 directions, if it is a triangle already and the expected bitmap for next column 

isComplete (_,_,_,a,_) = a
getSize    (a,_,_,_,_) = a
getExpect  (_,_,_,_,a) = a

matchesTriangle column (_,_,_,_,expected) = matches column expected

matchesDone (_,Done,Done,_,_) = True
matchesDone (_,_,_,_,_) = False

notMatchesDone x = not (matchesDone x)

filterTriangles column = filter  (matchesTriangle column)
filterDone = filter matchesDone
filterNotDone = filter notMatchesDone  

nextExpected Straight Straight l = l
nextExpected Up d (0:1:y) = 1:1:(nextExpected Straight d y)
nextExpected Down d (1:y) = 0:(nextExpected Straight d y)
nextExpected _ Up (1:0:y) = 0:0:y
nextExpected _ Down (1:0:y) = 1:1:y
nextExpected _ Up ([1]) = [0]
nextExpected c d (b:y) = b:(nextExpected c d y)
nextExpected _ _ [] = []

-- Generators

-- Generate from a column
-- arguments: column, bits on left, bits on the right
-- returns a list of partial triangles that can start from that column

-- base equations 
generateFromColumn [1] 0 0 = [(1, Done, Done, True, [])]
generateFromColumn _ 0  0= []
generateFromColumn [] _  _ = []
-- general case where column has a zero 
generateFromColumn (0:col) a n = generateFromColumn col (a+1) (n-1)
-- base cases where column has a one
------ a one on the top and a one on the bottom
generateFromColumn (1:col) 0 n = (1, Straight, Down, True, (makeZerosOnes 0 2)++(makeZerosOnes (n-1) 0)): (generateFromColumn col 1 (n-1))
generateFromColumn (1:col) m 0 = [(1, Up, Straight, True,  (makeZerosOnes (m-1) 2))]
------ a one in the middle, but after a 0
generateFromColumn (1:col) m n = (1, Up, Straight, True, (makeZerosOnes (m-1) 2)++(makeZerosOnes n 0)) 
                                     : (1, Straight, Down, True, (makeZerosOnes m 2)++(makeZerosOnes (n-1) 0)) 
                                     : (1, Up, Down, True, (makeZerosOnes (m-1) 3)++(makeZerosOnes (n-1) 0)) 
                                     : (generateFromColumn col (m+1) (n-1))
                                     
-- Continue generation from a partial triangle
-- given a partial triangle produces a list of expected partial triangles that are possible continuations of it 
-- First the increasing cases
-- base positive case
continueTriangle (a, _, _, amI, expected)
   | (ones expected) == 1    = [(a + 1, Done, Done, True, [])]
continueTriangle (a, Up, Down, True, expected)
   | ((head expected) == 1) || ((last expected) == 1) = [(a + (ones expected), Done, Done, True, [])]
   | otherwise                                        = [(a + (ones expected), Up, Down, True, (nextExpected Up Down expected)) ]  
continueTriangle (a, Up, Straight, True, expected)
   | ((head expected) == 1) = [(a + (ones expected), Down, Straight, False, (nextExpected Down Straight expected)), (a+(ones expected), Done, Done, True, [])]
   | otherwise              = [(a + (ones expected), Down, Straight, False, (nextExpected Down Straight expected)), (a+(ones expected), Done, Done, True, []), (a + (ones expected), Up, Straight, True, (nextExpected Up Straight expected))]
continueTriangle (a, Straight, Down, True, expected)
   | ((last expected) == 1) = [(a + (ones expected), Straight, Up, False, (nextExpected Straight Up expected)), (a+(ones expected), Done, Done, True, [])]
   | otherwise              = [(a + (ones expected), Straight, Up, False, (nextExpected Straight Up expected)), (a+(ones expected), Done, Done, True, []), (a + (ones expected), Straight, Down, True, (nextExpected Straight Down expected))]
-- Second the decreasing cases (all conform to the same equation)                                                 
continueTriangle (a, dir1, dir2, amI, expected) = [(a + (ones expected), dir1, dir2, amI, (nextExpected dir1 dir2 expected))]
 
maxTriangleSize l = foldl max  0 (map  (getSize) ((filter isComplete) l))

-- Given a current maxsize, a list of partial triangles and a list of bit columns
-- computes the size of the maximum triangle obtained by continuing with those columns derived from the list
partialSolve currentMax currentResultList [] = currentMax
partialSolve currentMax currentResultList (column:list) 
    | otherwise     = let
                        newResultList = (foldl (++) [] (map continueTriangle (filterTriangles column currentResultList))) ++ (generateFromColumn column 0 ((length column)-1))
                        doneList = filterDone newResultList
                        maxDone = max (maxTriangleSize newResultList) currentMax
                        notDone = filterNotDone newResultList
                        newMax = max (maxTriangleSize notDone) maxDone
                      in 
                        partialSolve newMax notDone list     
                        
generateList [] = []
generateList list
   | (allZeros list) = []
   | otherwise       = (nextColumn list):(generateList (nextList list))

-- complete partial solver with the list and its reverse 
solves initMax initList list = max (partialSolve initMax initList list) (partialSolve initMax initList (reverse list))
                         
-- The entry point                        
solve list = solves 0 [] (generateList list) 

-- Unit Test
unitTest (result, list) = (solve list) == result  


twoTimesPlusOne x = 2*x + 1

