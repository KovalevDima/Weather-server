{-# LANGUAGE ScopedTypeVariables #-}

module Server.Tests where

import Config
import Server.ExternalAPI ( Weather )
import Data.Aeson ( decode, encode )
import Test.QuickCheck ( Testable(property), quickCheck, Arbitrary(arbitrary) )
import Generic.Random


runTests = do
        quickCheck configSerrializationTest


instance Arbitrary Config where
    arbitrary = genericArbitrary uniform
configSerrializationTest = property $ \(config :: Config) -> (decode . encode $ config) == Just config


-- instance Arbitrary Weather where
--     arbitrary = genericArbitrary uniform
-- weatherSerrializationTest = property $ \(weather :: Weather) -> (decode . encode $ weather) == Just weather
