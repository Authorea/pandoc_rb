{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.C.Utils where

import Control.Concurrent
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Either (EitherT(..))
import Foreign.C.String           (CStringLen, peekCStringLen, newCStringLen)
import System.Timeout             (timeout)
import Text.Pandoc                (Pandoc, Inline(..))
import Text.Pandoc.MediaBag       (MediaBag, extractMediaBag, mediaDirectory)
import Text.Pandoc.Walk           (walk)


-- | Apply a monadic action to the left of an `EitherT`
mapML :: Monad m => (a -> m b) -> EitherT a m c -> EitherT b m c
mapML !f !(EitherT m) = EitherT $ do
  m' <- m
  case m' of
    ( Left  x) ->     Left  <$> f x
    ~(Right y) -> return $ Right     y

-- | Apply a monadic action to the right of an `EitherT`
mapMR :: Monad m => (b -> m c) -> EitherT a m b -> EitherT a m c
mapMR !f !(EitherT m) = EitherT $ do
  m' <- m
  case m' of
    ( Left  x) -> return $ Left      x
    ~(Right y) ->     Right <$> f y


-- | This function forks a thread to both catch exceptions and better kill infinite loops that can't be caught with `timeout` alone.
-- For example, @timeout 10 (forever $ return ())@ will never time out, but @timeoutEitherT 10 (lift $ forever $ return ())@ will.
--
-- timeoutEitherT is strict within the created thread
timeoutEitherT :: Int -> EitherT CStringLen IO b -> EitherT CStringLen IO b
timeoutEitherT !us !(EitherT io) = EitherT $ do
  mvar   <- newEmptyMVar
  tid    <- io `forkFinally` (putMVar mvar $!)
  result <- timeout us (takeMVar mvar)
  case result of
    ( Nothing       )                          -> killThread tid >> Left <$> newCStringLen ("Pandoc timed out after " ++ show us ++ "microseconds")
    ~(Just noTimeout) -> case noTimeout of
                           ( Right successful) -> return successful
                           ~(Left  err       ) -> Left <$> newCStringLen ("Pandoc internal error: " ++ show err)


-- | Taken from pandoc's executable, adjust the paths and extract the media to the given directory
-- https://github.com/jgm/pandoc/blob/9849ba7fd744f529f063e0802a18fa18c8433eeb/src/Text/Pandoc/Class.hs#L385
extractMedia :: MediaBag -> FilePath -> Pandoc -> IO Pandoc
extractMedia media dir d =
  case [fp | (fp, _, _) <- mediaDirectory media] of
        []  -> return d
        fps -> do
          extractMediaBag True dir media
          return $ walk (adjustImagePath dir fps) d

-- | Also taken from pandoc's executable, adjust the image paths of inline elements
adjustImagePath :: FilePath -> [FilePath] -> Inline -> Inline
adjustImagePath dir paths (Image attr lab (src, tit))
   | src `elem` paths = Image attr lab (dir ++ "/" ++ src, tit)
adjustImagePath _ _ x = x

-- | Extract the media bag to the given directory, unless that directory string is empty
extractCMediabag :: CStringLen -> MediaBag -> Pandoc -> EitherT CStringLen IO Pandoc
extractCMediabag !(_, 0)  _        !pandoc = return pandoc
extractCMediabag !cPath  !mediaBag !pandoc = do
  nonEmptyPath <- lift $ peekCStringLen cPath
  lift $ extractMedia mediaBag nonEmptyPath pandoc


