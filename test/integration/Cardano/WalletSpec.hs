{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.WalletSpec
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), launch )
import Cardano.Wallet
    ( NewWallet (..), WalletLayer (..), mkWalletLayer )
import Cardano.Wallet.Primitive.Mnemonic
    ( EntropySize, entropyToMnemonic, genEntropy )
import Cardano.Wallet.Primitive.Model
    ( WalletName (..), currentTip )
import Cardano.Wallet.Primitive.Types
    ( SlotId (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel )
import Control.Monad
    ( (>=>) )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Test.Hspec
    ( Spec, after, before, it, shouldSatisfy )

import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.Network.HttpBridge as HttpBridge

spec :: Spec
spec = do
    before startBridge $ after closeBridge $ do
        it "A newly created wallet can sync with the chain" $ \(_, wallet) -> do
            mnemonicSentence <-
                entropyToMnemonic <$> genEntropy @(EntropySize 15)
            wid <- unsafeRunExceptT $ createWallet wallet NewWallet
                { mnemonic = mnemonicSentence
                , mnemonic2ndFactor = mempty
                , name = WalletName "My Wallet"
                , passphrase = mempty
                , gap = minBound
                }
            handle <- async (watchWallet wallet wid)
            threadDelay 5000000
            cancel handle
            tip <- currentTip <$> unsafeRunExceptT (getWallet wallet wid)
            tip `shouldSatisfy` (> SlotId 0 0)
  where
    port = 1337
    closeBridge = cancel . fst
    startBridge = do
        handle <- async $ launch
            [ Command "cardano-http-bridge"
                [ "start"
                , "--port", show port
                , "--template", "testnet"
                ]
                (return ())
            ]
        threadDelay 1000000
        (handle,) <$> (mkWalletLayer
            <$> MVar.newDBLayer
            <*> HttpBridge.newNetworkLayer "testnet" port)

unsafeRunExceptT :: (MonadFail m, Show e) => ExceptT e m a -> m a
unsafeRunExceptT = runExceptT >=> \case
    Left e ->
        fail $ "unable to perform expect IO action: " <> show e
    Right a ->
        return a
