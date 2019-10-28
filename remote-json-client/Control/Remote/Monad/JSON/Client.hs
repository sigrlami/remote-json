{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Control.Remote.Monad.JSON.Client
        ( clientSendAPI
        , clientSendAPIWithAlt
        ) where

import Control.Lens ((^.), (.~), (&))
import Control.Monad (void)
import Control.Natural
import Control.Remote.Monad.JSON (SendAPI(..))
import Data.Aeson
import Data.ByteString
import Network.Wreq

--------------------------------------------------------------------------------

-- Public APIs
-- http://www.raboof.com/projects/jayrock/demo.ashx
-- https://s1.ripple.com:51234/

-- | A way of building client 'SendAPI' support, using wreq.
clientSendAPI :: String -> (SendAPI :~> IO)
clientSendAPI url = wrapNT $ \ case
  Sync v -> do
          r <- asJSON =<< post url (toJSON v)
          return $ r ^. responseBody
  Async v -> do
          void $ post url (toJSON v)


clientSendAPIWithAlt :: String -> (SendAPI :~> IO)
clientSendAPIWithAlt url = wrapNT $ \ case
  Sync v -> do
          r  <- post url (toJSON v)
          rp <- asJSON $ alterContentTypeToJson r
          return $ rp ^. responseBody
  Async v -> do
          void $ post url (toJSON v)


-- | Some APIs return JSON valid content with wrong headers
--   Use this workaround and try to convert to proper headers
alterContentTypeToJson :: Response body -> Response body
alterContentTypeToJson r =
  if "text/" `isPrefixOf` (r ^. responseHeader "Content-Type")
    then r & responseHeader "Content-Type" .~ "application/json"
    else r
