main :: IO ()
main = do 
    putStrLn "Hello, world!"
    s <- getALine ""
    putStrLn s
    num <- getLine
    -- use read to convert to an Int
    print func num


getALine :: String -> IO String
getALine st = do
					c <- getChar
					if c == '\n'
						then return st
						else getALine (st ++ [c])

func :: Int -> Int
func n = 3 * n + 4 * n^2 - 4
				return n