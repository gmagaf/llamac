{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module Parser.ParserT (ParserT(..),
                       parserT, pureParserT,
                       get, put, evalParserT, runParserT,
                       throw, withExcept, catch
                       ) where

import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Control.Monad.Trans.Except as Except (ExceptT(ExceptT), throwE, catchE, runExceptT, withExceptT)
import qualified Control.Monad.Trans.State as State (StateT(StateT), State, get, put, evalStateT)
import qualified Control.Monad.Trans.State.Strict as S (State)
import Control.Monad.IO.Class (MonadIO)
import LLVM.IRBuilder (MonadIRBuilder (liftIRState),
                       MonadModuleBuilder (liftModuleState), ModuleBuilderState,)
import LLVM.IRBuilder.Monad (IRBuilderState)

-- This module defines the Parser monad transformer

-- The monad transformer definition
newtype ParserT e s m a = ParserT { getParserT :: Except.ExceptT e (State.StateT s m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

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

evalParserT :: Monad m => ParserT e s m a -> s -> m (Either e a)
evalParserT p = State.evalStateT (Except.runExceptT (getParserT p))

runParserT :: ParserT e s m a -> s -> m (Either e a, s)
runParserT ~(ParserT (Except.ExceptT (State.StateT f))) = f

-- Exception related
throw :: Monad m => e -> ParserT e s m a
throw = ParserT . Except.throwE

withExcept :: Monad m => (e -> e') -> ParserT e s m a -> ParserT e' s m a
withExcept f = ParserT . Except.withExceptT f . getParserT

catch :: Monad m => (e -> ParserT e' s m a) -> ParserT e s m a -> ParserT e' s m a
catch handle p = ParserT $ Except.catchE (getParserT p) (getParserT . handle)

instance MonadTrans (ParserT e s) where
  lift :: Monad m => m a -> ParserT e s m a
  lift ma = parserT f where
    f s = do
      a <- ma
      return (Right a, s)

instance (MonadIRBuilder m) => MonadIRBuilder (ParserT e s m) where
  liftIRState :: S.State IRBuilderState a -> ParserT e s m a
  liftIRState = ParserT . liftIRState

instance (MonadModuleBuilder m) => MonadModuleBuilder (ParserT e s m) where
  liftModuleState :: State.State ModuleBuilderState a -> ParserT e s m a
  liftModuleState = ParserT . liftModuleState
