import Cms
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import System.Directory
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  reportFilename <- getReportFilename
  deleteReport reportFilename
  spec <- testSpec "CMS spec" $ cmsSpec reportFilename
  defaultMain $ testGroup "Tests" [spec]

getReportFilename :: IO FilePath
getReportFilename = do
  now <- today
  pure $ "cms-" <> show now <> ".json"

deleteReport :: FilePath -> IO ()
deleteReport reportFilename = do
  fileExists <- liftIO $ doesFileExist reportFilename
  when fileExists (liftIO $ removeFile reportFilename)

cmsSpec :: FilePath -> Spec
cmsSpec reportFilename = describe "Cms.createCmsReport" $ do
  it "creates the report file" $ do
    createCmsReport
    fileExists <- doesFileExist reportFilename
    fileExists `shouldBe` True

  it "generates the correct data" $ do
    createCmsReport
    contents <- LBS.readFile reportFilename
    let cmsReport = Aeson.decode contents :: Maybe CmsReport
    case cmsReport of
      Just report -> do
        report.posts `shouldBe` 100
        report.users `shouldBe` 10
        report.meanPostsPerUser `shouldBe` 10
      Nothing -> expectationFailure "Just value expected"
