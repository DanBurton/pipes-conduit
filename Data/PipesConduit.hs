{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Data.PipesConduit (
  proxyAsConduit, proxyAsPipe,
  conduitAsProxy, pipeAsProxy
  ) where

import Data.Conduit.Internal (ConduitM(ConduitM))
import qualified Data.Conduit.Internal as Conduit

import Control.Proxy.Core.Fast (ProxyFast)
import qualified Control.Proxy.Core.Fast as ProxyFast

import Control.Proxy (Proxy)
import qualified Control.Proxy as Proxy

import Control.Proxy.Safe (ExceptionP, SafeIO)
import qualified Control.Proxy.Safe as Safe

import Control.Proxy.Parse (StateP)
import qualified Control.Proxy.Parse as Parse


import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans



proxyAsConduit :: Monad m => (() -> ProxyFast a' a () b m r) -> ConduitM a b m (Maybe r)
proxyAsConduit p = ConduitM (Monad.liftM eitherToMaybe pipe) where
  pipe = proxyAsPipe (p ())

  eitherToMaybe :: Either () r -> Maybe r
  eitherToMaybe = \case
    Left () -> Nothing
    Right (r :: r) -> Just r


proxyAsPipe :: Monad m => ProxyFast a' a () b m r -> Conduit.Pipe l a b u m (Either u r)
proxyAsPipe = \case
  ProxyFast.Request (_ :: a') onRespond ->
    Conduit.NeedInput (proxyAsPipe . onRespond) (\(u :: u) -> return (Left u))

  ProxyFast.Respond (b :: b) onRequest ->
    Conduit.HaveOutput (proxyAsPipe (onRequest ())) (return ()) b

  ProxyFast.M m ->
    Conduit.PipeM (Monad.liftM proxyAsPipe m)

  ProxyFast.Pure (r :: r) ->
    Conduit.Done (Right r)


conduitAsProxy :: Proxy p => ConduitM a b IO r -> () ->
                  ExceptionP (StateP [a] p) () (Maybe a) b' b SafeIO r
conduitAsProxy (ConduitM p) () = pipeAsProxy p


pipeAsProxy :: Proxy p => Conduit.Pipe a a b () IO r ->
               ExceptionP (StateP [a] p) () (Maybe a) b' b SafeIO r
pipeAsProxy = \case
  Conduit.HaveOutput next finalize (b :: b) ->
    Safe.onAbort id finalize (Proxy.respond b) >> pipeAsProxy next

  Conduit.NeedInput next upstreamDone ->
    Proxy.liftP Parse.draw >>= \case
      Nothing -> pipeAsProxy (upstreamDone ())
      Just (a :: a) -> pipeAsProxy (next a)

  Conduit.Done (r :: r) ->
    return r

  Conduit.PipeM m ->
    Safe.tryIO m >>= pipeAsProxy

  Conduit.Leftover next l ->
    Proxy.liftP (Parse.unDraw l) >> pipeAsProxy next
