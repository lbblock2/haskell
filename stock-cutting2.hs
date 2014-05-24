import Data.List

{-main :: IO()
main = do
				putStrLn "Enter a number to stock cut: "
				strNum <- getLine
				let num = read strNum :: Int
				print (stocksRequired num [(-) num 1, (-) num 2, (-) num 3])

stocksRequired :: Int -> [Int] -> Int
stocksRequired n ns = Minimum (select n (permute n ns []) [])

permute :: Int -> [Int] -> [[Int]] -> [[Int]]
permute len lens allPermutes = do
																perm <- lens
																if (perm > len) then putStrLn "The stock isn't long enouth!"
																(++) allPermutes permute (Delete alen lens))
																permute len lens allPermutes

select :: Int->[[Int]]->[Int]->[Int]
select num toutPermutes permStocks = (++) permStocks (greedy num oneStk:stk:toutPermutes stk:toutPermutes)

greedy :: Int->Int->[Int]->Int
greedy maxLen x xs = 
										|(((+) x y:xs) <= maxLen) = return 1 + greedy maxLen y xs
										|(((+) x y:xs) > maxLen) = delete x xs
																							  return 1 + greedy maxLen y xs-}
stocksRequired :: Int -> [Int] -> Int
stocksRequired n li = minimum $ map (greedy n) (getPerms li)
												--greedyWrapper n (getPerms li)
												--return (findMin greedyWrapper)

getPerms :: [Int] -> [[Int]]
getPerms [] = [[]]
getPerms liInts = do
										num <- liInts
										(map (num :) $ getPerms(delete num liInts))



--greedy :: Int -> [Int] -> Int
greedy = undefined
--greedy stkLen aPerm = foldl (greedhelper stkLen ) (0, 0) aPerm

greedhelper :: Int -> Int -> Int -> Int -> (Int, Int)
greedhelper ln len1 len2 numStks
														|((+) len1 len2) > ln = ((1 + numStks), ((-) ln len2))
														|((+) len1 len2) <= ln = (numStks, ((-) ln ((+) len1 len2)))

{-greedyWrapper :: Int -> [[Int]] -> [Int]
greedyWrapper n pList = do
													perm <- permList
													map (perm :) $ (greedy n perm)

greedy :: Int -> [Int] -> Int
greedy [] = [0]
greedy stkLen aPerm = do
											len = p:aPerm
											if (len > stkLen) then return 0
											else if ((+) len (greedy stkLen (delete p:aPerm aPerm)) <= stkLen) then return ((+) 0 ((+) len (greedy stkLen (delete len aPerm))))
											else return ((+) 1 (greedy stkLen (delete len aPerm)))

findMin :: [Int] -> Int
findMin [] = 0
findMin liIn = foldl min liIn (do {perm <- (getPerms liIn); findMin perm;})-}
{-


do
		perm <- (getPerms liIn)
		foldl min perm len:perm
of foldl min perm (do {len <- perm;})
		-}
--findLeastStock :: [Int]
-- <- for loop over all hte values in the list; then the next line tack that on to the output list
	-- Cowbell on Youtube: great sketch
--myList = [2 * x | x <- [1,3,5,7]]
--myList2 = do
	--					x <- [1,3,5,7]
		--				return (2 * x)