{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Redex (
    RedexT(..),
    Redex,
    runRedex,
    (<|>),
    term,
    reduce,
) where

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (MonadState(..))
import Control.Monad (MonadPlus(..))

newtype RedexT a m b = RedexT { runRedexT :: a -> m b }

term :: (Monad m) => RedexT a m a
term = RedexT return

reduce :: (Monad m) => RedexT a m b -> a -> RedexT a m b
reduce r a = RedexT $ \_ -> runRedexT r a

instance (Functor m) => Functor (RedexT a m) where
    fmap f r = RedexT $ \a -> fmap f $ runRedexT r a

instance (Applicative m) => Applicative (RedexT a m) where
    pure = RedexT . const . pure
    f <*> r = RedexT $ \a -> runRedexT f a <*> runRedexT r a

instance (Alternative m) => Alternative (RedexT a m) where
    empty = RedexT $ \_ -> empty
    x <|> y = RedexT $ \a -> runRedexT x a <|> runRedexT y a

instance (Monad m) => Monad (RedexT a m) where
    r >>= f = RedexT $ \a -> runRedexT r a >>= \b -> runRedexT (f b) a
    fail s = RedexT $ \_ -> fail s

instance (MonadPlus m) => MonadPlus (RedexT a m) where
    mzero = RedexT $ \_ -> mzero
    x `mplus` y = RedexT $ \a -> runRedexT x a `mplus` runRedexT y a

instance MonadTrans (RedexT a) where
    lift = RedexT . const

instance (MonadState s m) => MonadState s (RedexT a m) where
    get = lift get
    put = lift . put

type Redex a = RedexT a Identity

runRedex :: Redex a b -> a -> b
runRedex = (runIdentity .) . runRedexT
