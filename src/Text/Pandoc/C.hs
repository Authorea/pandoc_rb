{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Text.Pandoc.C where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(..))
import Data.Aeson (ToJSON(..), genericToEncoding, defaultOptions, genericToJSON, encode)
import Data.Bifunctor (bimap, second)
import Data.ByteString.Lazy hiding (putStrLn)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackMallocCStringLen, unsafeFinalize)
import Data.Monoid ((<>))
import Foreign.C.String (CStringLen, peekCStringLen, newCStringLen)
import Foreign.C.Types (CChar, CLong, CInt(..))
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Ptr (Ptr(..), FunPtr(..), freeHaskellFunPtr, castPtr)
import Foreign.Storable (peek, poke)
import Foreign.Storable.Tuple ()
import Prelude hiding (log)
import Text.Pandoc (ReaderOptions, Pandoc, WriterOptions, PandocError(..), Reader(..), Writer(..), writerMediaBag, getReader, getWriter, def)
import Text.Pandoc.MediaBag (MediaBag)
import Text.Parsec.Error (ParseError, Message(..), errorPos, errorMessages)
import GHC.Generics (Generic)
import Text.Parsec.Pos (SourcePos(..), SourceName, Line, Column, sourceName, sourceLine, sourceColumn)


-- | The `Reader` type modified for foreign C exports
type CReader = ReaderOptions -> CStringLen -> EitherT CStringLen IO (Pandoc, MediaBag)

-- | The `Writer` type modified for foreign C exports
type CWriter = WriterOptions -> Pandoc -> EitherT CStringLen IO CStringLen

-- | To log error locations to stdout, uncomment below
log :: Show a => String -> EitherT a IO b -> EitherT a IO b
-- log str = mapML $ \x -> print (unwords [str, show x]) >> return x
log _ = id

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
showPandocError = flip unsafeUseAsCStringLen return . toStrict . encode


-- | Convert a `Reader` to a `CReader`
mkCReader :: Reader -> CReader
mkCReader  (StringReader     reader) opts cstr = log "51" . mapML showPandocError . EitherT $ do
  str <- peekCStringLen cstr
  readerResult <- reader opts str
  return $ second (, mempty) readerResult
mkCReader ~(ByteStringReader reader) opts  str = log "55" . mapML showPandocError . EitherT $ do
  bs           <- unsafePackMallocCStringLen str -- needs to be finalized with: unsafeFinalize :: ByteString -> IO ()
  readerResult <- reader opts $ fromStrict bs
  unsafeFinalize bs
  return readerResult

-- | Convert a `Writer` to a `CWriter`
mkCWriter :: Writer -> CWriter
mkCWriter  (PureStringWriter   writer) opts pandoc = log "63" . lift $ newCStringLen $ writer opts pandoc
mkCWriter  (IOStringWriter     writer) opts pandoc = log "64" . lift $ writer opts pandoc >>= newCStringLen
mkCWriter ~(IOByteStringWriter writer) opts pandoc = log "65" . lift $ writer opts pandoc >>= flip unsafeUseAsCStringLen return . toStrict

-- | `getReader` for foreign C exports. Retrieve reader based on formatSpec (format+extensions).
getCReader :: CStringLen -> EitherT CStringLen IO CReader
getCReader cstr = log "69" $ do
  str <- lift . peekCStringLen $ cstr
  EitherT $ case getReader str of
    ( Left  err   ) ->     Left  <$> newCStringLen err
    ~(Right reader) -> return . Right $ mkCReader reader

-- | `getWriter` for foreign C exports. Retrieve writer based on formatSpec (format+extensions).
getCWriter :: CStringLen -> EitherT CStringLen IO CWriter
getCWriter cstr = log "77" $ do
  str <- lift . peekCStringLen $ cstr
  EitherT $ case getWriter str of
    ( Left  err   ) ->      Left <$> newCStringLen err
    ~(Right writer) -> return . Right $ mkCWriter writer

-- | The main conversion function.
-- Takes `ReaderOptions`, `WriterOptions`, reader's formatSpec,
-- writer's formatSpec, input and returns either an error string
-- or the output
convert :: ReaderOptions
        -> WriterOptions
        -> CStringLen                       -- ^ Reader specification
        -> CStringLen                       -- ^ Writer specification
        -> CStringLen                       -- ^ Input
        -> EitherT CStringLen IO CStringLen -- ^ Either an error string or the output
convert readerOpts writerOpts readerStr writerStr input = log "93" $ do
  reader       <- getCReader readerStr
  (pandoc, mediaBag) <- reader readerOpts input
  writer       <- getCWriter writerStr
  let writerOpts' = writerOpts { writerMediaBag = writerMediaBag writerOpts <> mediaBag }
  writer writerOpts' pandoc

-- | The `ReaderOptions` wired into the foreign export
readerOptions :: ReaderOptions
readerOptions = def

-- | The `WriterOptions` wired into the foreign export
writerOptions :: WriterOptions
writerOptions = def

-- | `CStringLen` for use by Ruby
type RStringLen = (Ptr CChar, CLong)

-- | Makes a function pointer to a `Ptr` freeing function
foreign import ccall "wrapper" mkFree :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

-- | Free the resulting type of `convert_hs`
freeResult :: forall a. IO (FunPtr (Ptr a  -> IO ()))
freeResult = mkFree $ \addr -> do
  (_, _, strAddr, _) <- peek (castPtr addr :: Ptr (CInt, FunPtr (), Ptr CChar, CLong))
  free strAddr
  free addr

foreign export ccall freeHaskellFunPtr :: FunPtr a -> IO ()

-- | Takes: reader, writer, input and returns (isSuccess, a function to free the output, output)
convert_hs :: Ptr RStringLen -> Ptr RStringLen -> Ptr RStringLen -> IO (Ptr (CInt, FunPtr (Ptr a -> IO ()), Ptr CChar, CLong))
convert_hs readerStr writerStr input = do
  readerStr' <- second fromEnum <$> peek readerStr
  writerStr' <- second fromEnum <$> peek writerStr
  input'     <- second fromEnum <$> peek input

  result <- fmap (bimap (second toEnum) (second toEnum)) . runEitherT $ convert readerOptions writerOptions readerStr' writerStr' input'
  let (success, (rstrPtr, rstrLen)) = either (0,) (1,) result

  freeResult' <- freeResult
  addr        <- malloc
  addr `poke` (success, freeResult', rstrPtr, rstrLen)
  (ss, _, _, _) <- peek addr
  if ss /= 0 && ss /= 1
     then print (success, result, rstrLen) >> peek addr >>= print >> undefined
     else return ()
  return addr

foreign export ccall convert_hs :: Ptr RStringLen -> Ptr RStringLen -> Ptr RStringLen -> IO (Ptr (CInt, FunPtr (Ptr a -> IO ()), Ptr CChar, CLong))

-- | Dummy main to dissuade the compiler from warning a lack of @_main@
main :: IO CInt
main = print "hi there! I'm main. you probably shouldn't see me." >> return 0

foreign export ccall main :: IO CInt

