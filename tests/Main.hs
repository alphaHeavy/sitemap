import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.Text.IO as TIO
import Test.Tasty
import Test.Tasty.HUnit
import Web.Sitemap
import Text.XML.Stream.Parse

testYahooFinance :: TestTree
testYahooFinance = testCase "Yahoo Finance" $ do
  x <- TIO.readFile "tests/examples/finance.yahoo.com"
  r <- parseSitemap x
  case r of
    Right (Sitemap x) -> assertBool "Wrong number of results" $ 5 == L.length x
    _ -> assertFailure "No Results Returned"

testYahooFinanceUrls :: TestTree
testYahooFinanceUrls = testCase "Yahoo Finance Urls" $ do
  x <- TIO.readFile "tests/examples/finance.yahoo.com.1"
  r <- parseSitemap x
  case r of
    Right (UrlSet x) -> assertBool "Wrong number of results" $ 10 == L.length x
    _ -> assertFailure "No Results Returned"

testBloombergNews :: TestTree
testBloombergNews = testCase "Bloomberg News" $ do
  x <- TIO.readFile "tests/examples/bloomberg_news.xml"
  r <- parseSitemap x
  case r of
    Right (UrlSet x) -> do
      print x
      assertBool "Wrong number of results" $ 258 == L.length x
    _ -> assertFailure "No Results Returned"

tests :: TestTree
tests = testGroup "All Tests" [testYahooFinance, testYahooFinanceUrls, testBloombergNews]

main :: IO ()
main = defaultMain tests
