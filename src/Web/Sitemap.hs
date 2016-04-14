{-# LANGUAGE OverloadedStrings #-}
module Web.Sitemap (
    parseEither
  , parseSitemap
  , parseUrlIndex
  , ChangeFrequency(..)
  , Sitemap(..)
  , SitemapUrl(..)
) where

import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString
import Data.Conduit
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Time.Format
import Data.Url
import Data.XML.Types
import Text.XML.Stream.Parse

data ChangeFrequency = Always | Hourly | Daily | Weekly | Monthly | Never

data SitemapUrl = SitemapUrl {
  suLocation :: FullyQualifiedUrl,
  suLastModified :: Maybe UTCTime,
  suChangeFrequency :: Maybe ChangeFrequency,
  suPriority :: Maybe Float
}

data Sitemap = Sitemap {
  sLocation :: FullyQualifiedUrl,
  sLastModified :: Maybe UTCTime
}

parseChangeFrequency :: MonadThrow m => ConduitM Event o m (Maybe ChangeFrequency)
parseChangeFrequency = tagNoAttr "changefreq" $ do
  freq <- content
  return $ case freq of
          "always" -> Always
          "hourly" -> Hourly
          "daily" -> Daily
          "weekly" -> Weekly
          "monthly" -> Monthly
          "never" -> Never

parsePriority :: MonadThrow m => ConduitM Event o m (Maybe Float)
parsePriority = tagNoAttr "priority" $ do
  p <- content
  return $ read $ T.unpack p

parseLocation :: MonadThrow m => ConduitM Event o m (Maybe FullyQualifiedUrl)
parseLocation = tagNoAttr "loc" $ do
  loc <- content
  return $ parseFullyQualifiedUrl loc

parseLastModified :: MonadThrow m => ConduitM Event o m (Maybe UTCTime)
parseLastModified = tagNoAttr "" $ do
  t <- content
  parseTimeM True defaultTimeLocale (T.unpack t) format
  where
    format = "%Y-%m-%dT%H:%M:%S%z"

parseSitemapItem :: MonadThrow m => ConduitM Event o m (Maybe Sitemap)
parseSitemapItem = tagNoAttr "sitemap" $ do
  Just loc <- parseLocation
  mod <- parseLastModified
  return $ Sitemap loc mod

parseSitemap :: MonadThrow m => ConduitM Event o m (Maybe [Sitemap])
parseSitemap = tagIgnoreAttrs "sitemapindex" $ many parseSitemapItem

parseSitemapUrl :: MonadThrow m => ConduitM Event o m (Maybe SitemapUrl)
parseSitemapUrl = tagNoAttr "url" $ do
  Just loc <- parseLocation
  mod <- parseLastModified
  freq <- parseChangeFrequency
  pri <- parsePriority
  return $ SitemapUrl loc mod freq pri

parseUrlIndex :: MonadThrow m => ConduitM Event o m (Maybe [SitemapUrl])
parseUrlIndex = tagIgnoreAttrs "urlset" $ many parseSitemapUrl

parseEither :: MonadThrow m => ConduitM Event o m (Maybe (Either [Sitemap] [SitemapUrl]))
parseEither = (return . maybe Nothing (Just . Left) =<< parseSitemap) `orE` (return . maybe Nothing (Just . Right) =<< parseUrlIndex)
