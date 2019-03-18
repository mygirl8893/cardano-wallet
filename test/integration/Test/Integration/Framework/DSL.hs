{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Test.Integration.Framework.DSL
    (
    -- * Scenario
      scenario
    , xscenario
    , pendingWith
    , Scenarios
    , Context(..)

    -- * Steps
    , Setup(..)
    , setup
    --,  request TODO: From Request.hs
    --,  request_ TODO
    --,  successfulRequest TODO
    --,  unsafeRequest TODO

    -- * Helpers
--    , ($-) TODO: From Request.hs
    , (</>)
    , (!!)
    , json

    , WalletClient (..) -- TODO: Remove
    ) where

import Prelude

import Control.Concurrent
    ( MVar, threadDelay )
import Control.Concurrent.Async
    ( race )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Generics.Internal.VL.Lens
    ( Lens' )
import Data.List
    ( (!!) )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import Language.Haskell.TH.Quote
    ( QuasiQuoter )
import Test.Hspec.Core.Spec
    ( SpecM, it, xit )
import Test.Integration.Framework.Scenario
    ( Scenario )
--import           Test.Hspec.Expectations.Lifted
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Test.Hspec.Core.Spec as H

--
newtype WalletClient (m :: * -> *) = WalletClient (m Int)
type BaseUrl = String
type Manager = ()

resetWalletState :: IO ()
resetWalletState = return ()

successfulRequest = undefined

class HasHttpClient ctx where
    httpClient :: Lens' ctx (WalletClient IO)
    httpClient = undefined

--
-- SCENARIO
--

-- Prior to starting integration tests, we setup a global context
-- and "prepare" a few faucet wallets which all contains some funds.
-- This helps speed up testing and isolate them.
data Context = Context
    { _faucets
        :: [FilePath]
        -- Already funded faucet wallets
    , _client
        :: WalletClient IO
        -- A handle to the underlying wallet backend server
    , _manager
        :: (BaseUrl, Manager)
        -- The underlying BaseUrl and Manager used by the Wallet Client
    } deriving (Generic)


-- | Just a type-alias to 'SpecM', like 'scenario'. Ultimately, everything is
-- made in such way that we can use normal (albeit lifted) HSpec functions and
-- utilities if needed (and rely on its CLI as well when needed).
type Scenarios ctx = SpecM (MVar ctx) ()

-- | Just a slightly-specialized alias for 'it' to help lil'GHC. Also, makes
-- sure the wallet has been cleared out completely before running the scenario.
scenario
    :: String
    -> Scenario Context IO ()
    -> Scenarios Context
scenario title spec = it title (successfulRequest resetWalletState >> spec)

xscenario
    :: String
    -> Scenario Context IO ()
    -> Scenarios Context
xscenario = xit

-- | Lifted version of `H.pendingWith` allowing for temporarily skipping
-- scenarios from execution with a reason, like:
--
--      scenario title $ do
--          pendingWith "This test fails due to bug #213"
--          test
pendingWith
    :: (MonadIO m, MonadFail m)
    => String
    -> m ()
pendingWith = liftIO . H.pendingWith


--
-- STEPS
--

data Setup = Setup --TODO: Add record fields
    deriving (Show, Generic)

data Fixture = Fixture --TODO: Add record fields
    deriving (Show, Generic)

-- | Setup a wallet with the given parameters.
setup
    :: Setup
    -> Scenario Context IO Fixture
setup _args = return Fixture

--
-- HELPERS
--

json :: QuasiQuoter
json = aesonQQ

infixr 5 </>
(</>) :: ToHttpApiData a => Text -> a -> Text
base </> next = mconcat [base, "/", toQueryParam next]


--
-- INTERNALS
--

wantedSuccessButError
    :: (MonadFail m, Show e)
    => e
    -> m void
wantedSuccessButError =
    fail . ("expected a successful response but got an error: " <>) . show

wantedErrorButSuccess
    :: (MonadFail m, Show a)
    => a
    -> m void
wantedErrorButSuccess =
    fail . ("expected an error but got a successful response: " <>) . show

timeout :: (MonadIO m) => Int -> IO a -> m (Maybe a)
timeout maxWaitingTime action = liftIO $ do
    race (threadDelay maxWaitingTime) action >>= \case
        Left _  -> return Nothing
        Right a -> return (Just a)

second :: Int
second = 1000000

