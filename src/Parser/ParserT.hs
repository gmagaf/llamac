{-# Language GeneralizedNewtypeDeriving #-}
module Parser.ParserT (ParserT(..),
                       parserT, pureParserT,
                       get, put, eval, run,
                       throw, withExcept, catch
                       ) where

import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Control.Monad.Trans.Except as Except (ExceptT(ExceptT), throwE, catchE, runExceptT, withExceptT)
import qualified Control.Monad.Trans.State as State (StateT(StateT, runStateT), get, put, evalStateT)
import Control.Monad.IO.Class (MonadIO)

-- This module defines the Parser monad transformer

-- The monad transformer definition
newtype ParserT e s m a = ParserT { getParserT :: Except.ExceptT e (State.StateT s m) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    )

-- Constructors
parserT :: (s -> m (Either e a, s)) -> ParserT e s m a
parserT = ParserT . Except.ExceptT . State.StateT

pureParserT :: Monad m => (s -> (Either e a, s)) -> ParserT e s m a
pureParserT f = ParserT . Except.ExceptT . State.StateT $ (return . f)

-- State related
get :: Monad m => ParserT e s m s
get = ParserT $ lift State.get

put :: Monad m => s -> ParserT e s m ()
put s = ParserT $ lift (State.put s)

eval :: Monad m => s -> ParserT e s m a -> m (Either e a)
eval s p = State.evalStateT (Except.runExceptT (getParserT p)) s

run :: s -> ParserT e s m a -> m (Either e a, s)
run s p = State.runStateT (Except.runExceptT (getParserT p)) s

-- Exception related
throw :: Monad m => e -> ParserT e s m a
throw = ParserT . Except.throwE

withExcept :: Monad m => (e -> e') -> ParserT e s m a -> ParserT e' s m a
withExcept f = ParserT . Except.withExceptT f . getParserT

catch :: Monad m => (e -> ParserT e' s m a) -> ParserT e s m a -> ParserT e' s m a
catch handle p = ParserT $ Except.catchE (getParserT p) (getParserT . handle)
