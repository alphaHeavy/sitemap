{-# LANGUAGE DeriveGeneric #-}

module Web.Sitemap.Types where

import Control.DeepSeq
import Data.Text (Text)
import Data.Time
import Data.Url
import GHC.Generics

data ChangeFrequency = Always | Hourly | Daily | Weekly | Monthly | Never deriving (Enum,Eq,Generic,Show)

data SitemapUrl = SitemapUrl {
  suLocation :: FullyQualifiedUrl,
  suLastModified :: Maybe UTCTime,
  suChangeFrequency :: Maybe ChangeFrequency,
  suPriority :: Maybe Float,
  suNews :: Maybe News
} deriving (Generic,Show)

data SitemapItem = SitemapItem {
  sLocation :: FullyQualifiedUrl,
  sLastModified :: Maybe UTCTime
} deriving (Generic, Show)

data Publication = Publication {
  pName :: Text,
  pLanguage :: Text
} deriving (Eq,Generic,Show)

data News = News {
  nPublication :: Publication,
  nGenres :: Maybe Text,
  nPublicationDate :: UTCTime,
  nTitle :: Text,
  nKeywords :: Maybe Text,
  nStockTickers :: Maybe Text
} deriving (Generic,Show)

data SitemapResult = Sitemap [SitemapItem] | UrlSet [SitemapUrl] deriving (Generic,Show)

instance NFData SitemapResult
instance NFData News
instance NFData Publication
instance NFData SitemapItem
instance NFData SitemapUrl
instance NFData ChangeFrequency
