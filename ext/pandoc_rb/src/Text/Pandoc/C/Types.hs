{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.C.Types where

import Control.Monad.Trans.Either (EitherT(..))
import Data.Aeson                 (ToJSON(..), genericToEncoding, defaultOptions, genericToJSON, encode)
import Data.ByteString.Lazy       (toStrict)
import Data.ByteString            (useAsCStringLen)
import Foreign.C.String           (CStringLen)
import Foreign.C.Types            (CChar, CLong(..))
import Foreign.Ptr                (Ptr)
import GHC.Generics               (Generic)
import Text.Pandoc                (ReaderOptions, Pandoc, WriterOptions(..), PandocError(..))
import Text.Pandoc.MediaBag       (MediaBag)
import Text.Parsec.Error          (ParseError, Message(..), errorPos, errorMessages)
import Text.Parsec.Pos            (SourcePos, SourceName, Line, Column, sourceName, sourceLine, sourceColumn)


-- | The `Reader` type modified for foreign C exports
type CReader = ReaderOptions -> CStringLen -> EitherT CStringLen IO (Pandoc, MediaBag)

-- | The `Writer` type modified for foreign C exports
type CWriter = WriterOptions -> Pandoc -> EitherT CStringLen IO CStringLen


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


-- | `CStringLen` for use by Ruby
type RStringLen = (Ptr CChar, CLong)




