import Data.List

main :: IO ()
main = do
				putStrLn "Give me a stock-length: "
				strLen <- getLine
				let stkLen = read strLen :: Int
				putStrLn "Please enter a list of Ints"
				strList <- getLine
				let li = read strList :: [Int]
				--if working with pure values in a monad, can use let binding to run computation
				print (stocksRequired stkLen li)

stocksRequired :: Int -> [Int] -> Int
stocksRequired n li = minimum $ numStocks
																	where numStocks = map (numStksReqd n) $ (getPerms li)

getPerms :: [Int] -> [[Int]]
getPerms [] = [[]]
getPerms liInts = do
										num <- liInts
										(map (num :) $ getPerms(delete num liInts))

numStksReqd :: Int -> [Int] -> Int
numStksReqd stkLen perm = fst (foldl (transformState stkLen) (0, 0) perm)

transformState :: Int -> (Int, Int) -> Int -> (Int, Int)
transformState stkLen (nUsed, nLeft) len
																			| len > stkLen = error "sorry, too long, no can do"
																			|len <= nLeft = (nUsed, (nLeft - len))
																			|len > nLeft = ((nUsed + 1), (stkLen - len))