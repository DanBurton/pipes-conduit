{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Data.PipesConduit (
  proxyAsConduit, proxyAsPipe,
  conduitAsProxy, pipeAsProxy
  ) where

import qualified Data.Conduit.Internal as Conduit
import           Data.Conduit.Internal (ConduitM(ConduitM))

import qualified Control.Proxy.Core.Fast as ProxyFast
import           Control.Proxy.Core.Fast (ProxyFast)

import Control.Proxy (Proxy)
import qualified Control.Proxy as Proxy

import Control.Proxy.Safe (ExceptionP, SafeIO)
import qualified Control.Proxy.Safe as Safe

import Control.Proxy.Parse (StateP)
import qualified Control.Proxy.Parse as Parse

import Control.Monad (liftM)
import qualified Control.Monad.Trans.Class as Trans



proxyAsConduit :: Monad m => (() -> ProxyFast a' a () b m r) -> ConduitM a b m r
proxyAsConduit p = ConduitM (proxyAsPipe (p ()))

proxyAsPipe :: Monad m => ProxyFast a' a () b m r -> Conduit.Pipe l a b () m r
proxyAsPipe = \case
  ProxyFast.Request _ onRespond ->
    Conduit.NeedInput (proxyAsPipe . onRespond) (\_ -> return undefined) -- TODO!
  ProxyFast.Respond (b :: b) onRequest ->
    Conduit.HaveOutput (proxyAsPipe (onRequest ())) (return ()) b
  ProxyFast.M m ->
    Conduit.PipeM (liftM proxyAsPipe m)
  ProxyFast.Pure (r :: r) ->
    Conduit.Done r

conduitAsProxy :: Proxy p => ConduitM a b IO r -> () -> ExceptionP (StateP [a] p) () a b' b SafeIO r
conduitAsProxy (ConduitM p) () = pipeAsProxy p

pipeAsProxy :: Proxy p => Conduit.Pipe l a b () IO r -> ExceptionP (StateP [l] p) () a b' b SafeIO r
pipeAsProxy = \case
  Conduit.HaveOutput next finalize (b :: b) ->
    Safe.finally id finalize (Proxy.respond b) >> pipeAsProxy next
  Conduit.NeedInput next ummm ->
    Proxy.request () >>= pipeAsProxy . next -- TODO: use ummm
  Conduit.Done (r :: r) ->
    return r
  Conduit.PipeM m ->
    Safe.tryIO m >>= pipeAsProxy
  Conduit.Leftover next l ->
    Proxy.liftP (Parse.unDraw l) >> pipeAsProxy next
