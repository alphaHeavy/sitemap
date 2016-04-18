{-# LANGUAGE OverloadedStrings #-}
module Web.Sitemap (
    parseSitemap

  , module Web.Sitemap.Types
) where

import Control.Exception (SomeException)
import qualified Data.List as L
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Url
import Text.XML
import Web.Sitemap.Types

getContent :: Element -> Maybe Text
getContent = safeHead . fmap (\ (NodeContent x) -> x) . L.filter isContent . elementNodes
  where
    safeHead [] = Nothing
    safeHead (x:_) = Just x

parsePriority :: Element -> Float
parsePriority = read . T.unpack . fromJust . getContent


parseChangeFrequency :: Element -> ChangeFrequency
parseChangeFrequency = parse . fromJust . getContent
  where
    parse "always" = Always
    parse "hourly" = Hourly
    parse "daily" = Daily
    parse "weekly" = Weekly
    parse "monthly" = Monthly
    parse "never" = Never


parseLocation :: Element -> FullyQualifiedUrl
parseLocation = parseFullyQualifiedUrl . L.head . fmap (\ (NodeContent x) -> x) . elementNodes

parseLastModified :: Monad m => Element -> m UTCTime
parseLastModified = parseTimeM True defaultTimeLocale format . T.unpack . fromJust . getContent
  where
    format = "%Y-%m-%dT%H:%M:%S%z"

parseUrlItem :: Monad m => Element -> m (Maybe SitemapUrl)
parseUrlItem el = do
  t <- case lastm of
         Just x -> return . Just =<< parseLastModified x
         Nothing -> return Nothing
  go loc t
  where
    go (Just l) t = do
      return $ Just $ SitemapUrl (parseLocation l) t (fmap parseChangeFrequency freq) (fmap parsePriority priority) Nothing
    go _ _ = return Nothing

    loc = L.find (\ x -> "{http://www.sitemaps.org/schemas/sitemap/0.9}loc" == elementName x) elements
    lastm = L.find (\ x -> "{http://www.sitemaps.org/schemas/sitemap/0.9}lastmod" == elementName x) elements
    freq = L.find (\ x -> "{http://www.sitemaps.org/schemas/sitemap/0.9}changefreq" == elementName x) elements
    priority = L.find (\ x -> "{http://www.sitemaps.org/schemas/sitemap/0.9}priority" == elementName x) elements
    elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes el



parseSitemapItem :: Monad m => Element -> m (Maybe SitemapItem)
parseSitemapItem el =
  case loc of
    Just x ->
      case lastm of
        Just y -> do
          t <- parseLastModified y
          return $ Just $ SitemapItem (parseLocation x) (Just t)
        Nothing -> return $ Just $ SitemapItem (parseLocation x) Nothing
    Nothing -> return $ Nothing
  where
    loc = L.find (\ x -> "{http://www.sitemaps.org/schemas/sitemap/0.9}loc" == elementName x) elements
    lastm = L.find (\ x -> "{http://www.sitemaps.org/schemas/sitemap/0.9}lastmod" == elementName x) elements
    elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes el

isElement :: Node -> Bool
isElement (NodeElement _) = True
isElement _ = False

isContent :: Node -> Bool
isContent (NodeContent _) = True
isContent _ = False

parseSitemap :: Monad m => Text -> m (Either SomeException SitemapResult)
parseSitemap text =
  case parseText def $ TL.fromStrict text of
    Left x -> return $ Left x
    Right x -> do
      let el = documentRoot x
      case elementName el of
        x | x == "{http://www.sitemaps.org/schemas/sitemap/0.9}sitemapindex" -> do
          let elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes  el
          r <- mapM parseSitemapItem elements
          return $ Right $ Sitemap $ catMaybes r
        x | x == "{http://www.sitemaps.org/schemas/sitemap/0.9}urlset" -> do
          let elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes  el
          r <- mapM parseUrlItem elements
          return $ Right $ UrlSet $ catMaybes r
