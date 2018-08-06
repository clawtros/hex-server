{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Server (main)

someFunc :: IO ()
someFunc = main