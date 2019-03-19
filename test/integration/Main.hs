module Main where

import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.MVar
    ( newMVar )

import Data.Aeson
    ( Value )
import Data.String
    ( fromString )
import Data.Text
    ( Text )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Prelude
import System.Process
    ( proc, withCreateProcess )
import Test.Hspec
    ( beforeAll, describe, hspec )

import qualified Data.Text as T

import Test.Integration.Framework.DSL
    ( Context (..), Scenarios, expectError, scenario, unsafeRequest, verify )
import Test.Integration.Framework.Request
    ( RequestException (..) )

withWallet :: ((Text, Manager) -> IO a) -> IO a
withWallet action = do
    let launch = proc "cardano-wallet-server" testMnemonic
        testMnemonic = ["ring","congress","face","smile","torch","length","purse","bind","rule","reopen","label","ask","town","town","argue"]
        baseURL = T.pack "http://localhost:8090/"
    manager <- newManager defaultManagerSettings
    withCreateProcess launch $ \_ _ _ _ph -> do
        threadDelay 1000000
        action (baseURL, manager)

main :: IO ()
main = withWallet $ \http -> do
    hspec $ do
        beforeAll (newMVar $ Context () http) $ do
            describe "Dummy Request" dummySpec

dummySpec :: Scenarios Context
dummySpec = do
    scenario "Try the API which isn't implemented yet" $ do
        let endpoint = "api/wallets"
        response <- unsafeRequest ("GET", fromString endpoint) Nothing
        verify (response :: Either RequestException Value)
            [ expectError
            ]
