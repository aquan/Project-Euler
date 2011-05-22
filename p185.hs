-- Project Euler Problem 185: A 2nd Attempt in Haskell
-- 2011-05-19

-- Test set for babies
-- When run on the test set, should get 39542
test_guesses = [ "90342",
                 "70794",
                 "39458",
                 "34109",
                 "51545",
                 "12531" ]

test_corrects = [2, 0, 2, 1, 2, 1]

--zguesses = zip test_guesses test_corrects 

-- Real set for ballers
guesses = [ "5616185650518293",
            "3847439647293047",
            "5855462940810587",
            "9742855507068353",
            "4296849643607543",
            "3174248439465858",
            "4513559094146117",
            "7890971548908067",
            "8157356344118483",
            "2615250744386899",
            "8690095851526254",
            "6375711915077050",
            "6913859173121360",
            "6442889055042768",
            "2321386104303845",
            "2326509471271448",
            "5251583379644322",
            "1748270476758276",
            "4895722652190306",
            "3041631117224635",
            "1841236454324589",
            "2659862637316867" ]

corrects = [2, 1, 3, 3, 3, 1, 2, 3, 1, 2, 3, 1, 1, 2, 0, 2, 2, 3, 1, 3, 3, 2]

zguesses = zip guesses corrects

-- General approach:
-- Start with a string with no numeric characters. Now pick a guess, and
-- generate all strings such that the intersection of those strings and
-- the guess string equals the number correct in that guess exactly.
-- Filter out those that have intersections with all other guesses that 
-- exceed the number correct for each of those guesses, as they are not valid
-- solutions. Recurse through all guesses. Filter remaining candidates to find
-- the actual solution by checking that the intersection of each candidate
-- with all guesses matches the number correct EXACTLY. Fucken' celebrate.

-- Optimizations to implement:
-- 1) Start with strings with a few number correct, to prevent explosion in the
--    first couple of iterations. Have to learn to sort shit in Haskell.

-- Notes:
-- 1) I used indexing from 1. This is to avoid weird shit going on in replace.
--    I think. It might be re-indexable.
-- 2) zguesses is hardcoded in. It's gay but whatever. Just uncomment the
--    one that you want.

------------------------
---- Main Functions ----
------------------------

-- Doesn't exceed budget of intersections for all guesses.
-- Used to clip invalid candidate solutions at the bud.
safeAll x = length zguesses == length (filter (safeOne x) zguesses)              
            where safeOne x guess = intersect x (fst guess) <= snd guess

-- Has exactly the correct number of intersections for all guesses.
-- If this passes, we have a winner.
correctAll x =  length zguesses == length (filter (correctOne x) zguesses)
               where correctOne x guess = intersect x (fst guess) == snd guess

-- Solve this bitch
solve xs [] = filter (safeAll) xs -- return the correct solution
solve xs (g:gs) = solve safe gs -- recurse
                  where safe = filter safeAll new_candidates
                        new_candidates = concatMap (satisfy g) xs

-- Assign digits of x so that is satisfies guess g
satisfy guess x = setK x g k
              where g = fst guess
                    k = snd guess - intersect x g

-- Set k free digits of x to digits of g
setK x _ 0 = [x] -- this is the one that fucked me
setK x g k = map (subK x g) indexes
             where indexes = genSub k (nonNumIndexes x)

-- Substitute elements ks from y into x
-- Index from 1 for simplicity or something
subK x y [] = x -- just in case
subK x y [k] = take k' x ++ [y !! k'] ++ drop k x
               where k' = k - 1
subK x y (k:ks) = subK x' y ks
                  where x' = subK x y [k]


-----------------------------
---- Other useful things ----
-----------------------------

-- Check if a letter
isAlpha :: Char -> Bool
isAlpha x = x `elem` ['a'..'z'] ++ ['A'..'Z']

-- Check if a number
isNum :: String -> Bool
isNum x = x `elem` map show [1..]

-- Size of intersection of two strings
-- Assumes strings are of the same length
intersect :: Eq a => [a] -> [a] -> Int
intersect x y = sum [ 1 | i <- l, x !! i == y !! i ]
                where l = [0 .. length x - 1]

-- Find non-numeric indexes of strings
-- (these will be the ones that are free to be assigned)
-- Indexed from 1
nonNumIndexes :: [Char] -> [Int]
nonNumIndexes x = [ k + 1 | k <- l, isAlpha (x !! k) ]
                where l = [0 .. length x - 1]

-- Generate sublists of size k
genSub :: Int -> [a] -> [[a]]
genSub 1 xs = [ [x] | x <- xs ]
genSub k [] = []
genSub k (x:xs) = map (x:) (genSub (k-1) xs) ++ genSub k xs
