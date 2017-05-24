{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Text.Pandoc
import Text.Pandoc.C
import Test.QuickCheck
import Control.Monad.Trans.Either
import Foreign.C.String
import Data.Either
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Bifunctor


prop_mapMLIgnoresRight0 :: Bool
prop_mapMLIgnoresRight0 = mapML (const $ Just ()) (EitherT (Just (Right True))) == EitherT (Just (Right True))

prop_mapMLIgnoresRight1 :: Bool
prop_mapMLIgnoresRight1 = mapML (const $ Nothing) (EitherT (Just (Right True))) == (EitherT (Just (Right True)) :: EitherT () Maybe Bool)

prop_mapMLAppliesFunction0 :: Bool
prop_mapMLAppliesFunction0 = mapML (const $ Just ()) (EitherT (Just (Left 0))) == (EitherT (Just (Left ())) :: EitherT () Maybe Bool)

prop_mapMLAppliesFunction1 :: Bool
prop_mapMLAppliesFunction1 = mapML (const $ Nothing) (EitherT (Just (Left (0 :: Int)))) == (EitherT Nothing :: EitherT () Maybe Bool)

prop_mapMRIgnoresRight0 :: Bool
prop_mapMRIgnoresRight0 = mapMR (const $ Just ()) (EitherT (Just (Left True))) == EitherT (Just (Left True))

prop_mapMRIgnoresRight1 :: Bool
prop_mapMRIgnoresRight1 = mapMR (const $ Nothing) (EitherT (Just (Left True))) == (EitherT (Just (Left True)) :: EitherT Bool Maybe ())

prop_mapMRAppliesFunction0 :: Bool
prop_mapMRAppliesFunction0 = mapMR (const $ Just ()) (EitherT (Just (Right (0 :: Int)))) == (EitherT (Just (Right ())) :: EitherT Bool Maybe ())

prop_mapMRAppliesFunction1 :: Bool
prop_mapMRAppliesFunction1 = mapMR (const $ Nothing) (EitherT (Just (Right (0 :: Int)))) == (EitherT Nothing :: EitherT Bool Maybe ())

instance Show (Bool -> Maybe Int) where
  show f = concat ["f False = ", show (f False), "; f True = ", show (f True)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (EitherT a Maybe b) where
  arbitrary = oneof [ return . EitherT $ Nothing
                    , (EitherT . Just . Left ) <$> arbitrary
                    , (EitherT . Just . Right) <$> arbitrary
                    ]

prop_mapMLisSwappedMapMR :: (Bool -> Maybe Int) -> EitherT Bool Maybe () -> Bool
prop_mapMLisSwappedMapMR f x = mapML f x == swapEitherT (mapMR f (swapEitherT x))

testGotCReader :: String -> Property
testGotCReader readerStr = ioProperty $ withCStringLen readerStr (fmap isRight . runEitherT . getCReader)

testGotCWriter :: String -> Property
testGotCWriter writerStr = ioProperty $ withCStringLen writerStr (fmap isRight . runEitherT . getCWriter)

allProps :: (Functor t, Foldable t, Testable prop) => (a -> prop) -> t a -> Property
allProps f = foldr (.&&.) (property True) . fmap f

allPropsIO :: (Applicative t, Traversable t, Testable prop) => (a -> IO prop) -> t a -> Property
allPropsIO f = ioProperty . fmap (foldr (.&&.) (property True)) . mapM f

prop_getCReaderGetsReaders :: Property
prop_getCReaderGetsReaders = allProps testGotCReader (fst <$> readers)

prop_getCWriterGetsWriters :: Property
prop_getCWriterGetsWriters = allProps testGotCWriter (fst <$> writers)

foreign import ccall "dynamic" freerFunc :: FunPtr (Ptr a -> IO ()) -> Ptr a -> IO ()

prop_convert_hsWorks :: Property
prop_convert_hsWorks = ioProperty $ do
  readerCStr <- newCStringLen "markdown"
  writerCStr <- newCStringLen "latex"
  inputCStr  <- newCStringLen "hi there!"
  readerRStr <- malloc
  writerRStr <- malloc
  inputRStr <- malloc
  readerRStr `poke` second toEnum readerCStr
  writerRStr `poke` second toEnum writerCStr
  inputRStr  `poke` second toEnum inputCStr
  resultPtr <- convert_hs readerRStr writerRStr inputRStr
  peek readerRStr >>= free . fst
  peek writerRStr >>= free . fst
  peek inputRStr  >>= free . fst
  free readerRStr
  free writerRStr
  free inputRStr
  (success, freer, output) <- peek resultPtr
  if success == 0
     then return False
     else do
       (freerFunc freer) (castPtr resultPtr)
       return True

return []
mainCheck :: IO Bool
mainCheck = $quickCheckAll

main :: IO ()
main = mainCheck >>= print

