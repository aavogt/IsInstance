{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import IsInstance
import Data.Proxy
import Test.Hspec

writeTryCxt ''Show
writeTryCxt ''Num
writeTryCxt ''Fractional

f1 n = ifHasInstance (Proxy :: Proxy '[Show,Fractional]) "NA" (\n -> show $ n/2) n


f2a n = ifHasInstance (Proxy :: Proxy Show) "NA" show n


#if __GLASGOW_HASKELL__ == 708
-- ghc-7.6 bug cannot infer the following type
-- f2b :: IsInstance Fractional t => t -> Either t t
#endif
f2b n = ifHasInstance (Proxy :: Proxy Fractional) (Left n) (\m -> Right (m/2)) n


-- f2 :: (IsInstance Fractional t, IsInstance Show t) => t -> String
f2 x = either ((++) "Left " . f2a) ((++) "Right " . f2a) (f2b x)

class C a where c :: a -> String

instance C Int where c _ = "c"
writeTryCxt ''C


main = hspec $ do
  it "f1" $ do
    f1 (1 :: Int) `shouldBe` "NA"
    f1 (1 :: Double) `shouldBe` "0.5"
    f1 ((),'x') `shouldBe` "NA"

  it "f2" $ do
    f2 (1 :: Int) `shouldBe` "Left 1"
    f2 (1 :: Double) `shouldBe` "Right 0.5"
    f2 ('1' : ) `shouldBe` "Left NA"
    f2 ('x','y') `shouldBe` "Left ('x','y')"
