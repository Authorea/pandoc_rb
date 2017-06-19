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
import Data.Aeson                 (ToJSON(..), genericToEncoding, defaultOptions, genericToJSON, encode)
import Data.Bifunctor             (bimap, second)
import Data.ByteString.Lazy       (toStrict, fromStrict)
import Data.ByteString            (useAsCStringLen)
import Data.ByteString.Unsafe     (unsafePackMallocCStringLen, unsafeFinalize)
import Data.Monoid                ((<>))
import Foreign.C.String           (CStringLen, peekCStringLen, newCStringLen)
import Foreign.C.Types            (CChar, CLong, CInt(..))
import Foreign.Marshal.Alloc      (free, malloc)
import Foreign.Ptr                (Ptr, castPtr)
import Foreign.Storable           (peek, poke)
import Foreign.Storable.Tuple     ()
import GHC.Generics               (Generic)
import Prelude hiding             (log)
import Text.Pandoc                (ReaderOptions, Pandoc, WriterOptions(..), PandocError(..), Reader(..), Writer(..), HTMLMathMethod(..), writerMediaBag, getReader, getWriter, def)
import Text.Pandoc.MediaBag       (MediaBag, extractMediaBag)
import Text.Parsec.Error          (ParseError, Message(..), errorPos, errorMessages)
import Text.Parsec.Pos            (SourcePos, SourceName, Line, Column, sourceName, sourceLine, sourceColumn)
import System.Timeout             (timeout)
import Control.Concurrent




-- | The `Reader` type modified for foreign C exports
type CReader = ReaderOptions -> CStringLen -> EitherT CStringLen IO (Pandoc, MediaBag)

-- | The `Writer` type modified for foreign C exports
type CWriter = WriterOptions -> Pandoc -> EitherT CStringLen IO CStringLen


-- | Apply a monadic action to the left of an `EitherT`
mapML :: Monad m => (a -> m b) -> EitherT a m c -> EitherT b m c
mapML f (EitherT m) = EitherT $ do
  m' <- m
  case m' of
    ( Left  x) ->     Left  <$> f x
    ~(Right y) -> return $ Right     y

-- | Apply a monadic action to the right of an `EitherT`
mapMR :: Monad m => (b -> m c) -> EitherT a m b -> EitherT a m c
mapMR f (EitherT m) = EitherT $ do
  m' <- m
  case m' of
    ( Left  x) -> return $ Left      x
    ~(Right y) ->     Right <$> f y


-- | This function forks a thread to both catch exceptions and better kill infinite loops that can't be caught with `timeout` alone.
-- For example, @timeout 10 (forever $ return ())@ will never time out, but @timeoutEitherT 10 (lift $ forever $ return ())@ will.
--
-- timeoutEitherT is strict within the created thread
timeoutEitherT :: Int -> EitherT CStringLen IO b -> EitherT CStringLen IO b
timeoutEitherT us (EitherT io) = EitherT $ do
  mvar   <- newEmptyMVar
  tid    <- io `forkFinally` (putMVar mvar $!)
  result <- timeout us (takeMVar mvar)
  case result of
    ( Nothing       )                          -> killThread tid >> Left <$> newCStringLen ("Pandoc timed out after " ++ show us ++ "microseconds")
    ~(Just noTimeout) -> case noTimeout of
                           ( Right successful) -> return successful
                           ~(Left  err       ) -> Left <$> newCStringLen ("Pandoc internal error: " ++ show err)



-- | A clone of `SourcePos`, to allow deriving `Generic`
data SourcePos' = SourcePos' SourceName !Line !Column deriving (Eq, Ord, Show, Generic)

instance ToJSON SourcePos' where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic Message

instance ToJSON Message where
  toEncoding = genericToEncoding defaultOptions

-- | A clone of `ParseError`, to allow deriving `Generic`
data ParseError' = ParseError' !SourcePos' [Message] deriving (Generic)

-- | Convert to our clone of `SourcePos`
sourcePos' :: SourcePos -> SourcePos'
sourcePos' !sp = SourcePos' (sourceName sp) (sourceLine sp) (sourceColumn sp)

-- | Convert to our clone of `ParseError`
parseError' :: ParseError -> ParseError'
parseError' !pe = ParseError' (sourcePos' (errorPos pe)) (errorMessages pe)


instance ToJSON ParseError where
  toJSON = genericToJSON defaultOptions . parseError'
  toEncoding = genericToEncoding defaultOptions . parseError'

instance ToJSON PandocError where
    toEncoding = genericToEncoding defaultOptions

-- | Convert a `PandocError` to JSON
showPandocError :: PandocError -> IO CStringLen
showPandocError = flip useAsCStringLen return . toStrict . encode


-- | Convert a `Reader` to a `CReader`
mkCReader :: Reader -> CReader
mkCReader  (StringReader     reader) opts cstr = mapML showPandocError . EitherT $ do
  str          <- peekCStringLen cstr
  readerResult <- reader opts str
  return $ second (, mempty) $! readerResult
mkCReader ~(ByteStringReader reader) opts  str = mapML showPandocError . EitherT $ do
  bs           <- unsafePackMallocCStringLen str
  readerResult <- reader opts $! fromStrict bs
  readerResult `seq` unsafeFinalize bs
  return readerResult


-- | Convert a `Writer` to a `CWriter`
mkCWriter :: Writer -> CWriter
mkCWriter  (PureStringWriter   writer) opts pandoc = lift $ newCStringLen $ writer opts pandoc
mkCWriter  (IOStringWriter     writer) opts pandoc = lift $ writer opts pandoc >>= newCStringLen
mkCWriter ~(IOByteStringWriter writer) opts pandoc = lift $ writer opts pandoc >>= flip useAsCStringLen return . toStrict

-- | `getReader` for foreign C exports. Retrieve reader based on formatSpec (format+extensions).
getCReader :: CStringLen -> EitherT CStringLen IO CReader
getCReader cstr = do
  str <- lift . peekCStringLen $ cstr
  EitherT $ case getReader str of
    ( Left  err   ) ->     Left  <$> newCStringLen err
    ~(Right reader) -> return . Right $ mkCReader reader

-- | `getWriter` for foreign C exports. Retrieve writer based on formatSpec (format+extensions).
getCWriter :: CStringLen -> EitherT CStringLen IO CWriter
getCWriter cstr = do
  str <- lift . peekCStringLen $ cstr
  EitherT $ case getWriter str of
    ( Left  err   ) ->      Left <$> newCStringLen err
    ~(Right writer) -> return . Right $ mkCWriter writer

extractCMediabag :: Bool -> CStringLen -> MediaBag -> EitherT CStringLen IO ()
extractCMediabag verbose cPath mediaBag = do
  path <- lift . peekCStringLen $ cPath
  case path of
    []           -> return ()
    nonEmptyPath -> lift $ extractMediaBag verbose nonEmptyPath mediaBag

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
convert readerOpts writerOpts readerStr writerStr input mediaBagStr = do
  reader             <- getCReader readerStr
  (pandoc, mediaBag) <- reader readerOpts input
  extractCMediabag False mediaBagStr mediaBag
  writer             <- getCWriter writerStr
  let writerOpts'    = writerOpts { writerMediaBag = writerMediaBag writerOpts <> mediaBag }
  writer writerOpts' pandoc

-- | The `ReaderOptions` wired into the foreign export
readerOptions :: ReaderOptions
readerOptions = def

-- | The `WriterOptions` wired into the foreign export
writerOptions :: WriterOptions
writerOptions = def { writerHTMLMathMethod = MathJax "https://fail.cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML" }

-- | `CStringLen` for use by Ruby
type RStringLen = (Ptr CChar, CLong)

-- -- | Free the resulting type of `convert_hs`
-- freeResult :: Ptr () -> IO ()
-- freeResult !addr = do
--   (_, strAddr, _) <- peek (castPtr addr :: Ptr (CInt, Ptr CChar, CLong))
--   free strAddr
--   free addr

-- foreign export ccall freeResult :: Ptr () -> IO ()

-- | Takes: reader, writer, input and returns (isSuccess, output pointer, output length)
convert_hs :: Ptr RStringLen -> Ptr RStringLen -> Ptr RStringLen -> Ptr RStringLen -> IO (Ptr (CInt, Ptr CChar, CLong))
convert_hs readerStr writerStr input mediaBagStr = do
  readerStr'                        <- second fromEnum <$> peek readerStr
  writerStr'                        <- second fromEnum <$> peek writerStr
  input'                            <- second fromEnum <$> peek input
  mediaBagStr'                      <- second fromEnum <$> peek mediaBagStr
  let converted                     =  convert readerOptions writerOptions readerStr' writerStr' input' mediaBagStr'
  let EitherT safeConverted         =  timeoutEitherT (10 * {- minutes -} 60000000) converted
  result                            <- bimap (second toEnum) (second toEnum) <$> safeConverted
  let (success, (rstrPtr, rstrLen)) =  either (0,) (1,) result
  addr                              <- malloc
  addr `poke` (success, rstrPtr, rstrLen)
  return addr

foreign export ccall convert_hs :: Ptr RStringLen -> Ptr RStringLen -> Ptr RStringLen -> Ptr RStringLen -> IO (Ptr (CInt, Ptr CChar, CLong))

-- -- | Dummy main to dissuade the compiler from warning a lack of @_main@
-- main :: IO CInt
-- main = print "hi there! I'm main. you probably shouldn't see me." >> return 0

-- foreign export ccall main :: IO CInt

