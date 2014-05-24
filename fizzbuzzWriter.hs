import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a, w) } deriving (Show, Eq, Read, Ord)

instance (Monoid w) => Monad (Writer w) where
	return x = Writer (x, mempty)
	(Writer (x, u) ) >>= f = let (Writer (y, v) ) = f x in Writer (y, u `mappend` v)


reverseFizzBuzz :: Int -> Writer [String] Int
reverseFizzBuzz 1 = Writer (1, [fizzBuzz 1])
reverseFizzBuzz i = Writer (i - 1, [fizzBuzz i]) >>= reverseFizzBuzz

fizzBuzz :: Int -> String
fizzBuzz n 
					|(n `mod` 3 == 0 && n `mod` 5 == 0) = "FizzBuzz"
					|(n `mod` 3 == 0) = "Fizz"
					|(n `mod` 5 == 0) = "Buzz"
					|otherwise = show n