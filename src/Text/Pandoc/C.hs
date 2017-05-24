{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Pandoc.C where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Data.Bifunctor
import Data.ByteString.Lazy hiding (putStrLn)
import Data.ByteString.Unsafe
import Data.Monoid ((<>))
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple
import Text.Pandoc
import Text.Pandoc.MediaBag


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

-- | Convert a `Reader` to a `CReader`
mkCReader :: Reader -> CReader
mkCReader  (StringReader     reader) opts cstr = mapML (newCStringLen . show) . EitherT $ do
  str <- peekCStringLen cstr
  readerResult <- reader opts str
  return $ second (, mempty) readerResult
mkCReader ~(ByteStringReader reader) opts str = mapML (newCStringLen . show) . EitherT $ do
  bs     <- unsafePackMallocCStringLen str -- needs to be finalized with: unsafeFinalize :: ByteString -> IO ()
  readerResult <- reader opts $ fromStrict bs
  unsafeFinalize bs
  return readerResult

-- | Convert a `Writer` to a `CWriter`
mkCWriter :: Writer -> CWriter
mkCWriter  (PureStringWriter   writer) opts pandoc = lift $ newCStringLen $ writer opts pandoc
mkCWriter  (IOStringWriter     writer) opts pandoc = lift $ writer opts pandoc >>= newCStringLen
mkCWriter ~(IOByteStringWriter writer) opts pandoc = lift $ writer opts pandoc >>= newCStringLen . show

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
convert readerOpts writerOpts readerStr writerStr input = do
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
  (_, _, (strAddr, _)) <- peek (castPtr addr :: Ptr (CInt, FunPtr (), RStringLen))
  free strAddr
  free addr

foreign export ccall freeHaskellFunPtr :: FunPtr a -> IO ()

-- | Takes: reader, writer, input and returns (isSuccess, a function to free the output, output)
convert_hs :: Ptr RStringLen -> Ptr RStringLen -> Ptr RStringLen -> IO (Ptr (CInt, FunPtr (Ptr a -> IO ()), RStringLen))
convert_hs readerStr writerStr input = do
  readerStr' <- second fromEnum <$> peek readerStr
  writerStr' <- second fromEnum <$> peek writerStr
  input'     <- second fromEnum <$> peek input

  result <- fmap (bimap (second toEnum) (second toEnum)) . runEitherT $ convert readerOptions writerOptions readerStr' writerStr' input'
  let (success, rstr) = either (0,) (1,) result

  freeResult' <- freeResult
  addr   <- malloc
  addr `poke` (success, freeResult', rstr)
  return addr

foreign export ccall convert_hs :: Ptr RStringLen -> Ptr RStringLen -> Ptr RStringLen -> IO (Ptr (CInt, FunPtr (Ptr a -> IO ()), RStringLen))

-- -- | Dummy main to dissuade the compiler from warning a lack of @_main@
-- main :: IO CInt
-- main = print "hi there! I'm main. you probably shouldn't see me." >> return 0

-- foreign export ccall main :: IO CInt

