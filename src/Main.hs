{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Lib ( runApp, configurate )


main :: IO ()
main = do
        config <- configurate
        runApp config