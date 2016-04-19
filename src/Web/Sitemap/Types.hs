module Web.Sitemap.Types where

import Data.Text (Text)
import Data.Time
import Data.Url

data ChangeFrequency = Always | Hourly | Daily | Weekly | Monthly | Never deriving (Enum,Eq,Show)

data SitemapUrl = SitemapUrl {
  suLocation :: FullyQualifiedUrl,
  suLastModified :: Maybe UTCTime,
  suChangeFrequency :: Maybe ChangeFrequency,
  suPriority :: Maybe Float,
  suNews :: Maybe News
} deriving Show

data SitemapItem = SitemapItem {
  sLocation :: FullyQualifiedUrl,
  sLastModified :: Maybe UTCTime
}

data Publication = Publication {
  pName :: Text,
  pLanguage :: Text
} deriving (Eq,Show)

data News = News {
  nPublication :: Publication,
  nGenres :: Maybe Text,
  nPublicationDate :: UTCTime,
  nTitle :: Text,
  nKeywords :: Maybe Text,
  nStockTickers :: Maybe Text
} deriving Show

data SitemapResult = Sitemap [SitemapItem] | UrlSet [SitemapUrl]
