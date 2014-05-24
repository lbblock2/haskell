main :: IO ()
main = do
				putStrLn "Enter a string: "
				inStr <- getLine
				st = read inStr :: [Char]
				putStrLn "Index to start: "
				inInd <- getLine
				ind = read inInd :: Int
				putStrLn "Length of Substring: "
				inK <- getLine
				numk = read inK :: Int
				print (substr2 st ind numk)
				

substr :: String -> Int -> Int -> String
substr str index k = getNext str [] index k
											where getNext st1 st2 i 
												|i == k = return st2
												|otherwise = foldl (+) ((+) i 1) (getNext ((+) st2 (st1 !! i)) ((+) i 1) k)


substr2 :: String -> Int -> Int -> String
substr2 str i k = foldl (getNext str []) i str
										where getNext st1 st2 index len 
											|index == len = st2
											|otherwise = getNext st1 (st2 ++ (st1 !! index)) ((+) index 1) k

{-
substr3 :: String -> Int -> Int -> String
substr3 str i k = foldl go i str
		where go =	
-}