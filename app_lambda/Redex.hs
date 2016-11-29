module Redex (
    Redex(..),
    (<|>),
    eval,
) where

import Data.Maybe (fromJust, isNothing)
import Control.Applicative (Applicative(..), Alternative(..))

type RedexT m a b = m a -> m b

newtype Redex e = Redex { unwrap :: RedexT Maybe e e }

instance Functor Redex where
    fmap f r = Redex $ \e -> undefined

instance Applicative Redex where
    pure = Redex . const . Just
    r <*> a = Redex $ \e -> undefined

instance Alternative Redex where
    empty = Redex $ \_ -> empty
    f <|> g = Redex $ \e -> unwrap f e <|> unwrap g e

eval :: Redex e -> e -> e
eval r e = fromJust $ until (isNothing . unwrap r) (unwrap r) (Just e)
