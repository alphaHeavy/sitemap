import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.Text.IO as TIO
import Test.Tasty
import Test.Tasty.HUnit
import Web.Sitemap
import Text.XML.Stream.Parse hiding (force)

testYahooFinance :: TestTree
testYahooFinance = testCase "Yahoo Finance" $ do
  x <- TIO.readFile "tests/examples/finance.yahoo.com"
  r <- parseSitemap x
  case r of
    Right (Sitemap x) -> assertBool "Wrong number of results" $ 5 == (L.length $ force x)
    _ -> assertFailure "No Results Returned"

testYahooFinanceUrls :: TestTree
testYahooFinanceUrls = testCase "Yahoo Finance Urls" $ do
  x <- TIO.readFile "tests/examples/finance.yahoo.com.1"
  r <- parseSitemap x
  case r of
    Right (UrlSet x) -> assertBool "Wrong number of results" $ 10 == (L.length $ force x)
    _ -> assertFailure "No Results Returned"

testBloombergNews :: TestTree
testBloombergNews = testCase "Bloomberg News" $ do
  x <- TIO.readFile "tests/examples/bloomberg_news.xml"
  r <- parseSitemap x
  case r of
    Right (UrlSet x) -> do
      assertBool "Wrong number of results" $ 258 == (L.length $ force x)
    _ -> assertFailure "No Results Returned"

testCnbcNews :: TestTree
testCnbcNews = testCase "CNBC News" $ do
  x <- TIO.readFile "tests/examples/cnbc.com.news"
  r <- parseSitemap x
  case r of
    Right (UrlSet x) -> do
      assertBool "Wrong number of results" $ 80 == (L.length $ force x)
    _ -> assertFailure "No Results Returned"

testMarketWatchNews :: TestTree
testMarketWatchNews = testCase "MarketWatch News" $ do
  x <- TIO.readFile "tests/examples/marketwatch.com.news"
  r <- parseSitemap x
  case r of
    Right (UrlSet x) -> do
      assertBool "Wrong number of results" $ 140 == (L.length $ force x)
    _ -> assertFailure "No Results Returned"

testSeekingAlphaNews :: TestTree
testSeekingAlphaNews = testCase "Seeking Alpha News" $ do
  x <- TIO.readFile "tests/examples/seekingalpha.com.news"
  r <- parseSitemap x
  case r of
    Right (UrlSet y) -> do
      assertBool "Wrong number of results" $ 675 == (L.length $ force y)
    _ -> assertFailure "No Results Returned"

testMarketWatch :: TestTree
testMarketWatch = testCase "MarketWatch" $ do
  x <- TIO.readFile "tests/examples/marketwatch.com"
  r <- parseSitemap x
  case r of
    Right (Sitemap x) -> do
      assertBool "Wrong number of results" $ 23 == (L.length $ force x)
    _ -> assertFailure "No Results Returned"

testMarketWatchSearch :: TestTree
testMarketWatchSearch = testCase "MarketWatch Search" $ do
  x <- TIO.readFile "tests/examples/marketwatch.com.search"
  r <- parseSitemap x
  case r of
    Right (UrlSet x) -> do
      assertBool "Wrong number of results" $ 40000 == (L.length $ force x)
    _ -> assertFailure "No Results Returned"

testTampaBay :: TestTree
testTampaBay = testCase "Tampa Bay" $ do
  x <- TIO.readFile "tests/examples/tampabay.com"
  r <- parseSitemap x
  case r of
    Right (UrlSet x) -> do
      assertBool "Wrong number of results" $ 164 == (L.length $ force x)
    _ -> assertFailure "No Results Returned"

testBloombergIndex :: TestTree
testBloombergIndex = testCase "Bloomberg Index" $ do
  x <- TIO.readFile "tests/examples/bloomberg_index.xml"
  r <- parseSitemap x
  case r of
    Right (Sitemap x) ->
      print $ L.length x
      assertBool "Wrong number of results" $ 5 == (L.length $ force x)
    _ -> assertFailure "No Results Returned"

tests :: TestTree
tests = testGroup "All Tests" [testYahooFinance, testYahooFinanceUrls, testBloombergNews,testCnbcNews,testMarketWatchNews,
                              testMarketWatch,testSeekingAlphaNews,testMarketWatchSearch,testTampaBay,testBloombergIndex]

main :: IO ()
main = defaultMain tests
