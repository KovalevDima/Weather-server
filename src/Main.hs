{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Server.App ( runApp, configurate )


main :: IO ()
main = do
        config <- configurate
        runApp config