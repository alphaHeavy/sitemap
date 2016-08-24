{-# LANGUAGE OverloadedStrings #-}
module Web.Sitemap (
    parseSitemap
  , module Web.Sitemap.Types
) where

import Control.Exception (catch,SomeException(..))
import Control.Monad.Trans
import qualified Data.List as L
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Url
import Text.XML
import Web.Sitemap.Types

safeHead [] = Nothing
safeHead (x:_) = Just x

getContent :: Element -> Maybe Text
getContent = safeHead . fmap (\ (NodeContent x) -> x) . L.filter isContent . elementNodes

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
  x <- parseDate $ fromJust pubDate
  return $ News pub genres x title keywords stocks -- title keywords stocks
  where
    elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes el
    pub = parsePublication $ fromJust $ L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}publication" == elementName x) elements
    genres = maybe Nothing id $ fmap getContent $ L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}genres" == elementName x) elements
    pubDate = L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}publication_date" == elementName x) elements
    title = fromJust $ fmap (maybe "" id . getContent) $ L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}title" == elementName x) elements
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

parseDate :: MonadIO m => Element -> m UTCTime
parseDate el = do
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
      x5 <- tryParse format5 input
      return $ L.head $ catMaybes [x1,x2,x3,x4,x5]

    format1 = "%Y-%m-%d"
    format2 = "%Y-%m-%dT%H:%M%Z"
    format3 = "%Y-%m-%dT%H:%M:%S%Z"
    format4 = "%Y-%m-%dT%H:%M:%S%Q%Z"
    format5 = "%Y-%m-%d %H:%M:%S%Q %Z"


parseLocation :: Element -> Maybe FullyQualifiedUrl
parseLocation = convert . fmap parseUrl . safeHead . fmap (\ (NodeContent x) -> x) . elementNodes
  where
    convert :: Maybe Url -> Maybe FullyQualifiedUrl
    convert (Just (FullyQualifiedUrl x)) = Just x
    convert _ = Nothing

parseUrlItem :: MonadIO m => Maybe Text -> Element -> m (Maybe SitemapUrl)
parseUrlItem namespace el = do
  t <- case lastm of
         Just x -> return . Just =<< parseDate x
         Nothing -> return Nothing
  go loc t
  where
    go (Just l) t = do
      n <- pn
      case parseLocation l of
        Just x -> return $ Just $ SitemapUrl x t (fmap parseChangeFrequency freq) (fmap parsePriority priority) n
        Nothing -> return Nothing
    go _ _ = return Nothing

    loc = L.find (\ x -> makeElementName namespace "loc" == elementName x) elements
    lastm = L.find (\ x -> makeElementName namespace "lastmod" == elementName x) elements
    freq = L.find (\ x -> makeElementName namespace "changefreq" == elementName x) elements
    priority = L.find (\ x -> makeElementName namespace "priority" == elementName x) elements
    news = L.find (\ x -> "{http://www.google.com/schemas/sitemap-news/0.9}news" == elementName x) elements
    elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes el

    pn = do
      case news of
        Just x -> do
          t <- parseNews x
          return $ Just t
        Nothing  -> return Nothing

makeElementName :: Maybe Text -> Text -> Name
makeElementName (Just namespace) element = fromString $ T.unpack $ T.concat ["{",namespace,"}",element]
makeElementName Nothing element = fromString $ T.unpack element

parseSitemapItem :: MonadIO m => Element -> m (Maybe SitemapItem)
parseSitemapItem el =
  case maybe Nothing parseLocation loc of
    Just x ->
      case lastm of
        Just y -> do
          t <- parseDate y
          return $ Just $ SitemapItem x (Just t)
        Nothing -> return $ Just $ SitemapItem x Nothing
    Nothing -> return $ Nothing
  where
    loc = L.find (\ x -> "{http://www.sitemaps.org/schemas/sitemap/0.9}loc" == elementName x) elements
    lastm =  L.find (\ x -> "{http://www.sitemaps.org/schemas/sitemap/0.9}lastmod" == elementName x) elements
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
          r <- mapM (parseUrlItem (Just "http://www.sitemaps.org/schemas/sitemap/0.9")) elements
          return $ Right $ UrlSet $ catMaybes r
        x | x == "{http://www.google.com/schemas/sitemap/0.9}urlset" -> do
          let elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes  el
          r <- mapM (parseUrlItem (Just "http://www.google.com/schemas/sitemap/0.9")) elements
          return $ Right $ UrlSet $ catMaybes r
        x | x == "urlset" -> do
          let elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes  el
          r <- mapM (parseUrlItem Nothing) elements
          return $ Right $ UrlSet $ catMaybes r
