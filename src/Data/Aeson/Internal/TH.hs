{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Data.Aeson.Internal.TH (
    letrecE,
    autoletE,
) where

import Data.IORef              (IORef, atomicModifyIORef, newIORef, readIORef)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Language.Haskell.TH     (varE, ExpQ, Name, Q, newName, runIO)
import System.IO.Unsafe        (unsafeInterleaveIO)

import qualified Data.Map as Map
import qualified Language.Haskell.TH.Syntax as TH

letrecE
    :: forall a. Ord a
    => ((a -> ExpQ) -> (a -> ExpQ))
    -> ((a -> ExpQ) -> ExpQ)
    -> ExpQ
letrecE f g = do
    ref <- runIO $ newIORef Map.empty
    expr <- g (loop ref)
    bindings <- runIO $ readIORef ref
    mkLet bindings expr
  where
    mkLet :: Map.Map a (Name, TH.Exp) -> TH.Exp -> ExpQ
    mkLet bindings expr = do
        return $ TH.LetE
            [ TH.ValD (TH.VarP name) (TH.NormalB code) []
            | (_, (name, code)) <- Map.toList bindings
            ]
            expr

    loop :: IORef (Map.Map a (Name, TH.Exp)) -> a -> ExpQ
    loop ref y = do
        memo <- runIO $ readIORef ref
        case Map.lookup y memo of
            Nothing -> do
                name <- newName $ "_let" ++ show (Map.size memo)
                _ <- mfix_ $ \yCode -> do
                    runIO $ atomicModifyIORef ref $ \m -> (Map.insert y (name, yCode) m, ())
                    f (loop ref) y
                varE name

            Just (name, _) ->
                varE name

-- | Better 'letE'.
autoletE
    :: Ord a
    => (a -> ExpQ)            -- ^ what bindings are
    -> ((a -> ExpQ) -> ExpQ)  -- ^ expression with a function to generate bindings
    -> ExpQ
autoletE f = letrecE (const f)

-------------------------------------------------------------------------------
-- MonadFix Q is not always there
-------------------------------------------------------------------------------

class MonadFix_ m where
    mfix_ :: (a -> m a) -> m a

instance MonadFix_ Q where
    mfix_ k = do
        m <- runIO newEmptyMVar
        ans <- runIO (unsafeInterleaveIO (takeMVar m))
        result <- k ans
        runIO (putMVar m result)
        pure result
    {-# INLINE mfix_ #-}
