{-# LANGUAGE PackageImports, NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module DetailedSpec (tests) where

import qualified Test.QuickCheck as Q
import Distribution.TestSuite as TS

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Data.Bifunctor
import Data.Either
import Distribution.TestSuite
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Test.QuickCheck
import Text.Pandoc
import Text.Pandoc.C
import Text.Pandoc.C.Types
import Text.Pandoc.C.Utils


instance Show (Bool -> Maybe Int) where
  show f = concat ["f False = ", show (f False), "; f True = ", show (f True)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (EitherT a Maybe b) where
  arbitrary = oneof [ return . EitherT $ Nothing
                    , (EitherT . Just . Left ) <$> arbitrary
                    , (EitherT . Just . Right) <$> arbitrary
                    ]



testGotCReader :: String -> Property
testGotCReader readerStr = ioProperty $ withCStringLen readerStr (fmap isRight . runEitherT . getCReader)

testGotCWriter :: String -> Property
testGotCWriter writerStr = ioProperty $ withCStringLen writerStr (fmap isRight . runEitherT . getCWriter)

allProps :: (Functor t, Foldable t, Testable prop) => (a -> prop) -> t a -> Property
allProps f = foldr (.&&.) (property True) . fmap f

allPropsIO :: (Applicative t, Traversable t, Testable prop) => (a -> IO prop) -> t a -> Property
allPropsIO f = ioProperty . fmap (foldr (.&&.) (property True)) . mapM f


foreign import ccall "dynamic" freerFunc :: FunPtr (Ptr a -> IO ()) -> Ptr a -> IO ()


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

-- prop_mapMLisSwappedMapMR :: (Bool -> Maybe Int) -> EitherT Bool Maybe () -> Bool
-- prop_mapMLisSwappedMapMR f x = mapML f x == swapEitherT (mapMR f (swapEitherT x))

prop_getCReaderGetsReaders :: Property
prop_getCReaderGetsReaders = allProps testGotCReader (fst <$> readers)

prop_getCWriterGetsWriters :: Property
prop_getCWriterGetsWriters = allProps testGotCWriter (fst <$> writers)

-- prop_convert_hsWorks :: Property
-- prop_convert_hsWorks = ioProperty $ do
--   readerCStr <- newCStringLen "markdown"
--   writerCStr <- newCStringLen "latex"
--   inputCStr  <- newCStringLen "hi there!"
--   readerRStr <- malloc
--   writerRStr <- malloc
--   inputRStr <- malloc
--   readerRStr `poke` second toEnum readerCStr
--   writerRStr `poke` second toEnum writerCStr
--   inputRStr  `poke` second toEnum inputCStr
--   resultPtr <- convert_hs readerRStr writerRStr inputRStr
--   peek readerRStr >>= free . fst
--   peek writerRStr >>= free . fst
--   peek inputRStr  >>= free . fst
--   free readerRStr
--   free writerRStr
--   free inputRStr
--   (success, freer, output) <- peek resultPtr
--   if success == 0
--      then return False
--      else do
--        (freerFunc freer) (castPtr resultPtr)
--        return True


testTimeoutEitherT :: Testable prop => Int -> (CStringLen -> IO prop) -> (a -> IO prop) -> EitherT CStringLen IO a -> Property
testTimeoutEitherT us ifLeft ifRight = ioProperty . eitherT ifLeft ifRight . timeoutEitherT us


prop_timeoutEitherT_times_out_on_0 :: Property
prop_timeoutEitherT_times_out_on_0 = testTimeoutEitherT 0 (\(ptr, _) -> free ptr >> return True) (\_->return False) $ return ()

prop_timeoutEitherT_times_out_on_loose_loop :: Property
prop_timeoutEitherT_times_out_on_loose_loop = testTimeoutEitherT 1 (\(ptr, _) -> free ptr >> return True) return $ lift $ mapM_ putStr (repeat "") >> return False

prop_timeoutEitherT_times_out_on_tight_loop :: Property
prop_timeoutEitherT_times_out_on_tight_loop = testTimeoutEitherT 1 (\(ptr, _) -> free ptr >> return True) (return . (== 0)) . lift . mfix $ (return $!)

prop_timeoutEitherT_does_not_time_out_on_result :: Property
prop_timeoutEitherT_does_not_time_out_on_result = testTimeoutEitherT 100 (\(ptr, _) -> free ptr >> return False) return $ return True

prop_timeoutEitherT_catches_exceptions :: Property
prop_timeoutEitherT_catches_exceptions = testTimeoutEitherT 100 (\(ptr, _) -> free ptr >> return True) (\_->return False) . lift . print $ (div 1 0 :: Int)

-- prop_timeoutEitherT_catches_errors :: Property
-- prop_timeoutEitherT_catches_errors = testTimeoutEitherT 100 (\(ptr, _) -> free ptr >> return True) (\_->return False) $ EitherT undefined

prop_timeoutEitherT_catches_lefts :: Property
prop_timeoutEitherT_catches_lefts = testTimeoutEitherT 100 (\(ptr, _) -> free ptr >> return True) (\_->return False) (EitherT $ Left <$> (newCStringLen ""))


toTSResult :: Q.Result -> TS.Result
toTSResult Q.Success {} = TS.Pass
toTSResult Q.GaveUp {} = TS.Fail "GaveUp"
toTSResult Q.Failure {Q.reason} = TS.Fail reason

runQuickCheck :: Q.Testable p => p -> IO TS.Progress
runQuickCheck prop = do
        qres <- Q.quickCheckWithResult Q.stdArgs {Q.maxSuccess = 30,
                                                  Q.maxSize = 20} prop
        return $ (Finished . toTSResult) qres

tests :: IO [Test]
tests = return [
  Test $ TestInstance (runQuickCheck prop_mapMLIgnoresRight0) "prop_mapMLIgnoresRight0" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_mapMLIgnoresRight1) "prop_mapMLIgnoresRight1" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_mapMLAppliesFunction0) "prop_mapMLAppliesFunction0" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_mapMLAppliesFunction1) "prop_mapMLAppliesFunction1" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_mapMRIgnoresRight0) "prop_mapMRIgnoresRight0" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_mapMRIgnoresRight1) "prop_mapMRIgnoresRight1" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_mapMRAppliesFunction0) "prop_mapMRAppliesFunction0" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_mapMRAppliesFunction1) "prop_mapMRAppliesFunction1" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_getCReaderGetsReaders) "prop_getCReaderGetsReaders" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_getCWriterGetsWriters) "prop_getCWriterGetsWriters" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_timeoutEitherT_times_out_on_0) "prop_timeoutEitherT_times_out_on_0" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_timeoutEitherT_does_not_time_out_on_result) "prop_timeoutEitherT_does_not_time_out_on_result" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_timeoutEitherT_catches_exceptions) "prop_timeoutEitherT_catches_exceptions" ["tag"] [] undefined,
  -- Test $ TestInstance (runQuickCheck prop_timeoutEitherT_catches_errors) "prop_timeoutEitherT_catches_errors" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_timeoutEitherT_catches_lefts) "prop_timeoutEitherT_catches_lefts" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_timeoutEitherT_times_out_on_loose_loop) "prop_timeoutEitherT_times_out_on_loose_loop" ["tag"] [] undefined,
  Test $ TestInstance (runQuickCheck prop_timeoutEitherT_times_out_on_tight_loop) "prop_timeoutEitherT_times_out_on_tight_loop" ["tag"] [] undefined
  ]




