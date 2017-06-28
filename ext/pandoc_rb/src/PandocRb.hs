{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PandocRb where

import Foreign.C.Types            (CChar, CLong(..), CInt(..))
import Foreign.Ptr                (Ptr)
import Text.Pandoc.C              (convert_hs, result_success, result_ptr, result_len, free_result)


foreign export ccall convert_hs     :: Ptr CChar -> CLong -> Ptr CChar -> CLong -> Ptr CChar -> CLong -> Ptr CChar -> CLong -> IO (Ptr (CInt, Ptr CChar, CLong))

foreign export ccall result_success :: Ptr (CInt, Ptr CChar, CLong) -> IO CInt

foreign export ccall result_ptr     :: Ptr (CInt, Ptr CChar, CLong) -> IO (Ptr CChar)

foreign export ccall result_len     :: Ptr (CInt, Ptr CChar, CLong) -> IO CLong

foreign export ccall free_result    :: Ptr (CInt, Ptr CChar, CLong) -> IO ()

