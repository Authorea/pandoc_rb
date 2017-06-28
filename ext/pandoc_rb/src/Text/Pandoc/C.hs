{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.C where

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Either (EitherT(..))
import Data.Bifunctor             (bimap, second)
import Data.ByteString            (useAsCStringLen)
import Data.ByteString.Lazy       (toStrict, fromStrict)
import Data.ByteString.Unsafe     (unsafePackCStringLen)
import Data.Monoid                ((<>))
import Foreign.C.String           (CStringLen, peekCStringLen, newCStringLen)
import Foreign.C.Types            (CChar, CLong(..), CInt(..))
import Foreign.Marshal.Alloc      (malloc, free)
import Foreign.Ptr                (Ptr)
import Foreign.Storable           (peek, poke)
import Foreign.Storable.Tuple     ()
import Text.Pandoc                (ReaderOptions, WriterOptions(..), Reader(..), Writer(..), HTMLMathMethod(..), writerMediaBag, getReader, getWriter, def)

import Text.Pandoc.C.Utils
import Text.Pandoc.C.Types


-- | Convert a `Reader` to a `CReader`
mkCReader :: Reader -> CReader
mkCReader  !(StringReader     reader) !opts !cstr = mapML showPandocError . EitherT $ do
  str          <- peekCStringLen cstr
  readerResult <- reader opts str
  return $ second (, mempty) $! readerResult
mkCReader ~(ByteStringReader  reader) !opts  str = mapML showPandocError . EitherT $ do
  bs           <- unsafePackCStringLen str
  readerResult <- reader opts $! fromStrict bs
  return readerResult


-- | Convert a `Writer` to a `CWriter`
mkCWriter :: Writer -> CWriter
mkCWriter !(PureStringWriter   writer) !opts !pandoc = lift $ newCStringLen (writer opts pandoc)
mkCWriter !(IOStringWriter     writer) !opts !pandoc = lift $ writer opts pandoc >>= newCStringLen
mkCWriter !(IOByteStringWriter writer) !opts !pandoc = lift $ writer opts pandoc >>= flip useAsCStringLen return . toStrict


-- | `getReader` for foreign C exports. Retrieve reader based on formatSpec (format+extensions).
getCReader :: CStringLen -> EitherT CStringLen IO CReader
getCReader !cstr = do
  str <- lift . peekCStringLen $! cstr
  EitherT $ case getReader str of
    ( Left  err   ) ->     Left  <$> newCStringLen err
    ~(Right reader) -> return . Right $! mkCReader reader

-- | `getWriter` for foreign C exports. Retrieve writer based on formatSpec (format+extensions).
getCWriter :: CStringLen -> EitherT CStringLen IO CWriter
getCWriter !cstr = do
  str <- lift $ peekCStringLen cstr
  EitherT $ case getWriter str of
    ( Left  err   ) ->      Left <$> newCStringLen err
    ~(Right writer) -> return . Right $! mkCWriter writer


-- | The main conversion function.
-- Takes `ReaderOptions`, `WriterOptions`, reader's formatSpec,
-- writer's formatSpec, input and returns either an error string
-- or the output
convert :: ReaderOptions
        -> WriterOptions
        -> CStringLen                       -- ^ Reader specification
        -> CStringLen                       -- ^ Writer specification
        -> CStringLen                       -- ^ Input
        -> CStringLen                       -- ^ Extract media dir
        -> EitherT CStringLen IO CStringLen -- ^ Either an error string or the output
convert !readerOpts !writerOpts !readerStr !writerStr !input !mediaBagStr = do
  reader             <- getCReader $! readerStr
  (pandoc, mediaBag) <- reader readerOpts $! input
  pandoc'            <- extractCMediabag mediaBagStr mediaBag pandoc
  writer             <- getCWriter $! writerStr
  let writerOpts'    = writerOpts { writerMediaBag = writerMediaBag writerOpts <> mediaBag }
  writer writerOpts' pandoc'


-- | The `ReaderOptions` wired into the foreign export
readerOptions :: ReaderOptions
readerOptions = def

-- | The `WriterOptions` wired into the foreign export
writerOptions :: WriterOptions
writerOptions = def { writerHTMLMathMethod = MathJax "https://fail.cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML" }


-- | Takes: reader, writer, input and returns (isSuccess, output pointer, output length)
convert_hs :: Ptr CChar                         -- ^ reader format pointer
           -> CLong                             -- ^ reader format length
           -> Ptr CChar                         -- ^ writer format pointer
           -> CLong                             -- ^ writer format length
           -> Ptr CChar                         -- ^ input pointer
           -> CLong                             -- ^ input length
           -> Ptr CChar                         -- ^ media extraction path pointer
           -> CLong                             -- ^ media extraction path length
           -> IO (Ptr (CInt, Ptr CChar, CLong)) -- ^ Pointer to @(success, output pointer, output length)@
convert_hs !readerStr !readerLen !writerStr !writerLen !input !inputLen !mediaBagStr !mediaBagLen = do
  let readerStr'                    =  (readerStr  , fromEnum readerLen  )
  let writerStr'                    =  (writerStr  , fromEnum writerLen  )
  let input'                        =  (input      , fromEnum inputLen   )
  let mediaBagStr'                  =  (mediaBagStr, fromEnum mediaBagLen)
  let converted                     =  convert readerOptions writerOptions readerStr' writerStr' input' mediaBagStr'
  let EitherT safeConverted         =  timeoutEitherT (10 * {- minutes -} 60000000) converted
  result                            <- bimap (second toEnum) (second toEnum) <$> safeConverted
  let (success, (rstrPtr, rstrLen)) =  either (0,) (1,) result
  addr                              <- malloc
  addr `poke` (success, rstrPtr, rstrLen)
  return addr


-- | Extract the success value from a result pointer
result_success :: Ptr (CInt, Ptr CChar, CLong) -> IO CInt
result_success addr = do
  (success, _, _) <- peek addr
  return success

-- | Extract the pointer to the result string from a result pointer
result_ptr :: Ptr (CInt, Ptr CChar, CLong) -> IO (Ptr CChar)
result_ptr addr = do
  (_, ptr, _) <- peek addr
  return ptr

-- | Extract the length (in bytes) of the result string from a result pointer
result_len :: Ptr (CInt, Ptr CChar, CLong) -> IO CLong
result_len addr = do
  (_, _, len) <- peek addr
  return len

-- | Free a result pointer. Note: This only frees the struct that wraps the
-- result values, it does not free the result string's memory
free_result :: Ptr (CInt, Ptr CChar, CLong) -> IO ()
free_result addr = do
  -- (_, ptr, _) <- peek addr -- this is commented out because the caller manages it
  -- free ptr
  free addr
  return ()


