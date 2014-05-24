main :: IO ()
main = putStrLn "Hello, world! This is a tree."

data Tree a = Nil | Node a (Tree a) (Tree a)


foldTree :: (a -> b -> a) -> a -> [b] -> a
foldTree f acc Nil = acc
foldTree tree = foldl (foldOp tree) acc tree
												where foldOp input 
												 |input.LeftChild == Nil = acc
												 |otherwise = foldl (foldOp input.RightChild) ((+) acc input.LeftChild) input

foldTree f acc (Node val lhs rhs) = foldTree f combinedAcc val
																	where combinedAcc = foldTree f newAcc rhs
																		where newAcc = foldTree f acc lhs