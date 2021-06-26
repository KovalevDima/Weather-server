{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Server.App ( runApp, configurate )
import           Server.Tests (runTests)

main :: IO ()
main = do
        runTests
        config <- configurate
        runApp config