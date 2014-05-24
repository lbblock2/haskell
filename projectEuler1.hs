main :: IO ()
main = do
	numIn = getLine
	num = read getLine Int
	print (sumNums num)

sumNums :: Int -> Int
sumNums x = factor x + sumNums (x `-` 1)

factor :: Int -> Int
facotr n = if (1000 `-` n) % 3 == 0
							then (1000 `-` n) 
							else if (1000 `-` n) % 5 == 0
								then (1000 `-` n)
							else 0)