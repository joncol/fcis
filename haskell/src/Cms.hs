module Cms
  ( CmsReport (..)
  , createCmsReport
  , today
  ) where

import Control.Applicative (empty)
import Control.Monad.Except
import Data.Aeson (parseJSON, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseMaybe)
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

data CmsError = HttpError Int | JsonError JSONException | NoRecords

instance Show CmsError where
  show (HttpError status) = "Error occurred, status = " <> show status
  show (JsonError err) = "Error occurred: " <> show err
  show NoRecords = "Error occurred: no records"

type CmsMonad = Either CmsError

createCmsReport :: IO ()
createCmsReport = do
  -- Prepare inputs, and fetch data from external source.
  putStrLn "Getting CMS data"
  response <- httpJSONEither cmsUrl
  now <- today
  let reportFilename = "cms-" <> show now <> ".json"

  -- Functional core, with error handling.
  let result = responseBody response >>= parseCmsReport

  -- Handle result, and write CMS report to file.
  handleResult reportFilename result

today :: IO Day
today = getCurrentTime >>= pure . utctDay

responseBody
  :: Response (Either JSONException Aeson.Value)
  -> CmsMonad Aeson.Value
responseBody response
  | httpStatus == 200 =
      case (getResponseBody response :: Either JSONException Aeson.Value) of
        Left err -> throwError $ JsonError err
        Right body -> pure body
  | otherwise = throwError $ HttpError httpStatus
  where
    httpStatus = getResponseStatusCode response

parseCmsReport :: Aeson.Value -> CmsMonad CmsReport
parseCmsReport records =
  maybe (throwError NoRecords) (pure . toReport) $ parseMaybe parseJSON records
  where
    toReport :: [Post] -> CmsReport
    toReport posts =
      let users = countUsers posts
      in  CmsReport
            { posts = length posts
            , users
            , meanPostsPerUser = length posts `div` users
            }
    countUsers =
      length . foldl (\acc post -> Set.insert (post.userId) acc) Set.empty

handleResult :: FilePath -> CmsMonad CmsReport -> IO ()
handleResult reportFilename (Right report) = do
  putStrLn $ "CMS data has: " <> show report.posts <> " records"
  Aeson.encodeFile reportFilename report
  putStrLn "Wrote CMS report"
handleResult _ (Left err) = do
  putStrLn $ show err
  exitFailure
