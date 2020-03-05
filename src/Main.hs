{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans (liftIO)
import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Web.Spock ((<//>))
import qualified Web.Spock as Spock
import qualified Web.Spock.Config as SC
-- import           Web.Spock.Lucid (lucid)
import qualified Lucid

type SessionState = ()
data ServerState  = DummyAppState (IORef Int)

main :: IO ()
main = do
    ref <- newIORef 0
    spockCfg <- SC.defaultSpockCfg () SC.PCNoDatabase (DummyAppState ref)
    Spock.runSpock 8080 (Spock.spock spockCfg app)

app :: Spock.SpockM () SessionState ServerState ()
app =
    let
        html :: Lucid.Html ()
        html = do
            Lucid.h1_ [Lucid.style_ "color:red"] "cookies?"
            Lucid.p_ [Lucid.style_ "font-weight:bold"] "how many?"
            Lucid.input_ []
    in do
    Spock.get Spock.root $ Spock.html $ TL.toStrict $ Lucid.renderText html

    Spock.get ("hello" <//> Spock.var) $
        \name -> do
            (DummyAppState ref) <- Spock.getState
            visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
            Spock.text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
