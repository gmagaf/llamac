{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser.ParserT (ParserT(..),
                       parserT, pureParserT,
                       evalParserT, runParserT,
                       throw, withExcept, catch
                       ) where

import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.State (MonadState (..))
import qualified Control.Monad.Trans.Except as Except (ExceptT(ExceptT), throwE, catchE, runExceptT, withExceptT)
import qualified Control.Monad.Trans.State as State (StateT(StateT), State, get, put, evalStateT, runState)
import qualified Control.Monad.Trans.State.Strict as S (State, runState)
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Lens (use, (.=))
import LLVM.IRBuilder (MonadIRBuilder (liftIRState),
                       MonadModuleBuilder (liftModuleState), ModuleBuilderState,)
import LLVM.IRBuilder.Monad (IRBuilderState)

import Parser.ParserState (ParserState, cgen_state)
import IR.CodeGenState (moduleState, irState)

-- This module defines the Parser monad transformer

-- The monad transformer definition
newtype ParserT e s m a = ParserT { getParserT :: Except.ExceptT e (State.StateT s m) a }

-- Constructors
parserT :: (s -> m (Either e a, s)) -> ParserT e s m a
parserT = ParserT . Except.ExceptT . State.StateT

pureParserT :: Monad m => (s -> (Either e a, s)) -> ParserT e s m a
pureParserT f = ParserT . Except.ExceptT . State.StateT $ (return . f)

-- State related
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

instance Functor f => Functor (ParserT e s f) where
  fmap :: forall a b. (a -> b) -> ParserT e s f a -> ParserT e s f b
  fmap f p = parserT (fmap g . runParserT p) where
    g :: (Either e a, s) -> (Either e b, s)
    g ~(a, s) = (fmap f a, s)

instance (Functor m, Monad m) => Applicative (ParserT e s m) where
  pure :: a -> ParserT e s m a
  pure a = parserT $ \s -> return (Right a, s)
  (<*>) :: forall a b. ParserT e s m (a -> b) -> ParserT e s m a -> ParserT e s m b
  pf <*> pa = parserT $ \s -> do
    ~(ef, s') <- runParserT pf s
    case ef of
      Left e  -> return (Left e, s')
      Right f -> do
        ~(ea, s'') <- runParserT pa s'
        case ea of
          Left e  -> return (Left e, s'')
          Right a -> return (Right (f a), s'')

instance Monad m => Monad (ParserT e s m) where
  return :: a -> ParserT e s m a
  return a = parserT $ \s -> return (Right a, s)
  (>>=) :: ParserT e s m a -> (a -> ParserT e s m b) -> ParserT e s m b
  p >>= f = parserT $ \s -> do
              ~(ea, s') <- runParserT p s
              case ea of
                Left e  -> return (Left e, s')
                Right a -> runParserT (f a) s'

instance MonadFix m => MonadFix (ParserT e s m) where
  mfix :: forall a. (a -> ParserT e s m a) -> ParserT e s m a
  mfix f = parserT mf where
    mf :: s -> m (Either e a, s)
    mf s = mfix (\ ~(ea, _) -> runParserT (f (either (const bomb) id ea)) s)
    bomb = error "mfix (ParserT): recursive computation returned Left value"

instance MonadIO m => MonadIO (ParserT e s m) where
  liftIO :: IO a -> ParserT e s m a
  liftIO io = ParserT (liftIO io)

instance Monad m => MonadState s (ParserT e s m) where
  get :: ParserT e s m s
  get = ParserT $ lift State.get
  put :: s -> ParserT e s m ()
  put s = ParserT $ lift (State.put s)

instance MonadTrans (ParserT e s) where
  lift :: Monad m => m a -> ParserT e s m a
  lift ma = parserT f where
    f s = do
      a <- ma
      return (Right a, s)

instance (Monad m) => MonadIRBuilder (ParserT e ParserState m) where
  liftIRState :: S.State IRBuilderState a -> ParserT e ParserState m a
  liftIRState s = do
    irs <- use (cgen_state . irState)
    let (a, irs') = S.runState s irs
    cgen_state . irState .= irs'
    return a

instance Monad m => MonadModuleBuilder (ParserT e ParserState m) where
  liftModuleState :: State.State ModuleBuilderState a -> ParserT e ParserState m a
  liftModuleState s = do
    ms <- use (cgen_state . moduleState)
    let ~(a, ms') = State.runState s ms
    cgen_state . moduleState .= ms'
    return a
