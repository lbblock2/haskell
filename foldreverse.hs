main :: IO ()
main = do
				--putStrLn "Please enter a list: "
				--strlist <- getLine
				--let list = read strlist :: [a]
				print foldReverse "string"


--foldReverse :: [a] -> [a]
--foldReverse list = extractLast list []
	--									where extractLast xs acc
		--									|acc == 0 = xs
			--								|otherwise = xs !! acc : extractLast (xs!!(acc - 1))


foldReverse :: [a] -> [a]
foldReverse list = foldl (rvs list [] 0) 0 list
															where rvs li1 li2 acc 
																| acc == last li1 = li2 ++ (li1 !! acc)
																| otherwise = rvs(li1 (li2 + li1 !! acc) acc + 1)
