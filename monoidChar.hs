import Data.Monoid
import Data.Char


instance Monoid Char where 
			mempty = chr 0
			mappend x y 
								|x >= y = x
								|otherwise = y