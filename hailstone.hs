main :: IO ()
main = do
				putStrLn "Please enter a number to Hailstone: "
				strnum <- getLine
				let num = read strnum :: Int
				print (hailstone num)

hailstone :: Int -> [Int]
hailstone 1 = [1]
hailstone n = if ((n `mod` 2) == 0)
								then n : (hailstone (n `div` 2))
								else n : (hailstone (3 * n + 1))
