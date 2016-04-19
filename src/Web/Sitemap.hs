{-# LANGUAGE OverloadedStrings #-}
module Web.Sitemap (
    parseSitemap

  , module Web.Sitemap.Types
) where

import Control.Exception (catch,SomeException(..))
import Control.Monad.Trans
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

parsePublication :: Element -> Publication
parsePublication el = go
  where
    go
      | Just n <- name,
        Just l <- lang = Publication (fromJust $ getContent n) (fromJust $ getContent l)
    name = L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}name" == elementName x) elements
    lang = L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}language" == elementName x) elements
    elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes el

parseNews :: MonadIO m => Element -> m News
parseNews el = do
  x <- parsePublicationDate $ fromJust pubDate
  return $ News pub genres x title keywords stocks -- title keywords stocks
  where
    elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes el
    pub = parsePublication $ fromJust $ L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}publication" == elementName x) elements
    genres = fmap (fromJust . getContent) $ L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}genres" == elementName x) elements
    pubDate = L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}publication_date" == elementName x) elements
    title = fromJust $ fmap (fromJust . getContent) $ L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}title" == elementName x) elements
    keywords = maybe Nothing id $ fmap getContent $  L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}keywords" == elementName x) elements
    stocks = maybe Nothing id $ fmap getContent $  L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}stock_tickers" == elementName x) elements

parseChangeFrequency :: Element -> ChangeFrequency
parseChangeFrequency = parse . fromJust . getContent
  where
    parse "always" = Always
    parse "hourly" = Hourly
    parse "daily" = Daily
    parse "weekly" = Weekly
    parse "monthly" = Monthly
    parse "never" = Never

parsePublicationDate :: MonadIO m => Element -> m UTCTime
parsePublicationDate el = do
  let (Just content) = getContent el
  tryAll $ T.unpack content
  where
    tryParse format input = (return . Just =<< parseTimeM True defaultTimeLocale format input) `catch` (\ (SomeException _) -> return Nothing)

    tryAll :: MonadIO m => String -> m UTCTime
    tryAll input = liftIO $ do
      x1 <- tryParse format1 input
      x2 <- tryParse format2 input
      x3 <- tryParse format3 input
      x4 <- tryParse format4 input
      return $ L.head $ catMaybes [x1,x2,x3,x4]

    format1 = "%Y-%m-%d"
    format2 = "%Y-%m-%dT%H:%M%z"
    format3 = "%Y-%m-%dT%H:%M:%S%z"
    format4 = "%Y-%m-%dT%H:%M:%S%Q%Z"

parseLocation :: Element -> FullyQualifiedUrl
parseLocation = parseFullyQualifiedUrl . L.head . fmap (\ (NodeContent x) -> x) . elementNodes

parseLastModified :: Monad m => Element -> m UTCTime
parseLastModified = parseTimeM True defaultTimeLocale format . T.unpack . fromJust . getContent
  where
    format = "%Y-%m-%dT%H:%M:%S%z"

parseUrlItem :: MonadIO m => Element -> m (Maybe SitemapUrl)
parseUrlItem el = do
  t <- case lastm of
         Just x -> return . Just =<< parseLastModified x
         Nothing -> return Nothing
  go loc t
  where
    go (Just l) t = do
      n <- pn
      return $ Just $ SitemapUrl (parseLocation l) t (fmap parseChangeFrequency freq) (fmap parsePriority priority) n
    go _ _ = return Nothing

    loc = L.find (\ x -> "{http://www.sitemaps.org/schemas/sitemap/0.9}loc" == elementName x) elements
    lastm = L.find (\ x -> "{http://www.sitemaps.org/schemas/sitemap/0.9}lastmod" == elementName x) elements
    freq = L.find (\ x -> "{http://www.sitemaps.org/schemas/sitemap/0.9}changefreq" == elementName x) elements
    priority = L.find (\ x -> "{http://www.sitemaps.org/schemas/sitemap/0.9}priority" == elementName x) elements
    news = L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}news" == elementName x) elements
    elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes el

    pn = do
      case news of
        Just x -> do
          t <- parseNews x
          return $ Just t
        Nothing  -> return Nothing


parseSitemapItem :: MonadIO m => Element -> m (Maybe SitemapItem)
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

parseSitemap :: MonadIO m => Text -> m (Either SomeException SitemapResult)
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
