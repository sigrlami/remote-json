{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad        
import Control.Monad.Remote.JSON.Strong
import Control.Applicative
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as IO
import System.Random

import Server

main = do
        s <- session
        t <- send s $ do
                command "say" [String "Hello!"]
                command "say" [String "Hello!"]
                t <- procedure "temperature" [] 
                return t
        print t                      
        
