module Cms
  ( CmsReport (..)
  , createCmsReport
  , today
  ) where

import Control.Applicative (empty)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Set qualified as Set
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Generics (Generic)
import Network.HTTP.Simple
import System.Exit (exitFailure)

cmsUrl :: Request
cmsUrl = "http://jsonplaceholder.typicode.com/posts"

data Post = Post
  { userId :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data CmsReport = CmsReport
  { posts :: Int
  , users :: Int
  , meanPostsPerUser :: Int
  }
  deriving stock (Show)

instance Aeson.FromJSON CmsReport where
  parseJSON (Aeson.Object o) = do
    posts <- o .: "posts"
    users <- o .: "users"
    meanPostsPerUser <- o .: "mean_posts_per_user"
    pure CmsReport {..}
  parseJSON _ = empty

instance Aeson.ToJSON CmsReport where
  toJSON CmsReport {..} =
    Aeson.object
      [ "posts" .= posts
      , "users" .= users
      , "mean_posts_per_user" .= meanPostsPerUser
      ]
  toEncoding CmsReport {..} =
    Aeson.pairs
      ( "posts" .= posts
          <> "users" .= users
          <> "mean_posts_per_user" .= meanPostsPerUser
      )

createCmsReport :: IO ()
createCmsReport = do
  putStrLn "Getting CMS data"
  response <- httpJSONEither cmsUrl
  let httpStatus = getResponseStatusCode response
  if httpStatus == 200
    then do
      case (getResponseBody response :: Either JSONException Aeson.Value) of
        Left err -> putStrLn $ "Error occurred: " <> show err
        Right records -> do
          case Aeson.parseMaybe Aeson.parseJSON records of
            Nothing -> putStrLn $ "Error occurred: no records"
            Just (posts :: [Post]) -> do
              putStrLn $ "CMS data has: " <> show (length posts) <> " records"
              now <- today
              let reportFilename = "cms-" <> show now <> ".json"
                  users =
                    foldl
                      (\acc post -> Set.insert (post.userId) acc)
                      Set.empty
                      posts
                  meanPostsPerUser = length posts `div` length users
              Aeson.encodeFile reportFilename $
                CmsReport
                  { posts = length posts
                  , users = length users
                  , meanPostsPerUser
                  }
              putStrLn "Wrote CMS report"
    else do
      putStrLn $ "Error occurred, status = " <> show httpStatus
      exitFailure

today :: IO Day
today = getCurrentTime >>= pure . utctDay
