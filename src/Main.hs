{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Text (Text)

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import Servant.Types.SourceT (foreach)
import Control.Lens

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Servant.Client.Streaming as S

type API = "topstories.json" :> Get '[JSON] [ItemId]
	:<|> "item" :> Capture "itemid" Integer  :> ".json" :> Get '[JSON] Value

newtype ItemId = ItemId Integer deriving Generic
instance FromJSON ItemId

data Story = Story
	{ _by :: Text
	, _descendants :: Integer
	, _id :: ItemId
	, _kids :: [ItemId]
	, _score :: Integer
	, _time :: Integer
	, _title :: Text
	, _type :: Text
	, _url :: Text } deriving Generic
makeLenses ''Story

instance FromJSON Story

storyFromValue :: Value -> Maybe Story
storyFromValue (Object assocs) = undefined
	where
	go :: (String, Value) -> (Story -> Story)
	go ("time", Number t)  = set time t
storyFromValue _ = Nothing

api :: Proxy API
api = Proxy

topstories :<|> item = client api

main :: IO ()
main = do
	manager' <- newManager tlsManagerSettings
	base_url <- parseBaseUrl "https://hacker-news.firebaseio.com/v0"
	--res <- runClientM topstories (mkClientEnv manager' base_url)
	res <- runClientM (item 19199647) (mkClientEnv manager' base_url)
	case res of
		Left err -> print err
		Right sts -> do
			print sts
			putStrLn "Hello, Haskell!"
