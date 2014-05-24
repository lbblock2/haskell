import Data.Monoid
import Control.Monad.State

hailstone2 :: Int -> [Int]
hailstone2 n = evalState hailstoneState ([], n)

hailstoneState = do
									(li, x) <- get
									if (x == 1)
										then return li
									else do
										put (li ++ [x] , x `div` 2)
										hailstoneState
										if (even x)
											then do
														put (li ++ [x] , x `div` 2)
														hailstoneState
											else do
														put (li ++ [x] , (3 * x + 1))
														hailstoneState
