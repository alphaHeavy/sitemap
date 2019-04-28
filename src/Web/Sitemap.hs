{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Sitemap (
    parseSitemap
  , module Web.Sitemap.Types
) where

import Control.Exception (catch,catches,Handler(..),throw,fromException, SomeException(..))
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

parseNews :: MonadIO m => Element -> m (Maybe News)
parseNews el = do
  x <- parseDate $ fromJust pubDate
  case x of
    Just y -> return $ Just $ News pub genres y title keywords stocks
    Nothing -> return Nothing
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

parseDate :: MonadIO m => Element -> m (Maybe UTCTime)
parseDate el = do
  let (Just content) = getContent el
  tryAll $ T.unpack content
  where
    tryParse format input = (return . Just =<< parseTimeM True defaultTimeLocale format input) `catch` (\ (SomeException _) -> return Nothing)

    tryAll :: MonadIO m => String -> m (Maybe UTCTime)
    tryAll input = liftIO $ do
      x1 <- tryParse format1 input
      x2 <- tryParse format2 input
      x3 <- tryParse format3 input
      x4 <- tryParse format4 input
      x5 <- tryParse format5 input
      x6 <- tryParse format6 input
      return $ safeHead $ catMaybes [x1,x2,x3,x4,x5,x6]


    format1 = "%Y-%m-%d"
    format2 = "%Y-%m-%dT%H:%M%Z"
    format3 = "%Y-%m-%dT%H:%M:%S%Z"
    format4 = "%Y-%m-%dT%H:%M:%S%Q%Z"
    format5 = "%Y-%m-%d %H:%M:%S%Q %Z"
    format6 = "%Y-%m-%dT%H:%M:%S%EZ"


parseLocation :: Element -> Maybe FullyQualifiedUrl
parseLocation = convert . fmap parseUrl . safeHead . fmap (\ (NodeContent x) -> x) . elementNodes
  where
    convert :: Maybe Url -> Maybe FullyQualifiedUrl
    convert (Just (FullyQualifiedUrl x)) = Just x
    convert _ = Nothing

parseUrlItem :: forall m. MonadIO m => Maybe Text -> Element -> m (Maybe SitemapUrl)
parseUrlItem namespace el = do
  t <- case lastm of
         Just x -> parseDate x
         Nothing -> return Nothing
  go loc t
  where
    go :: Maybe Element -> Maybe UTCTime -> m (Maybe SitemapUrl)
    go (Just l) t = do
      n :: (Maybe News) <- pn
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
        Just x -> parseNews x
        Nothing  -> return Nothing

makeElementName :: Maybe Text -> Text -> Name
makeElementName (Just namespace) element = fromString $ T.unpack $ T.concat ["{",namespace,"}",element]
makeElementName Nothing element = fromString $ T.unpack element

parseSitemapItem :: MonadIO m => Text -> Element -> m (Maybe SitemapItem)
parseSitemapItem namespace el =
  case maybe Nothing parseLocation loc of
    Just x ->
      case firstMaybe [lastm] of
        Just y -> do
          t <- parseDate y
          case t of
            Just z -> return $ Just $ SitemapItem x (Just z)
            Nothing -> return Nothing
        Nothing -> return $ Just $ SitemapItem x Nothing
    Nothing -> return $ Nothing
  where
    loc = L.find (\ x -> (fromString $ T.unpack $ T.concat ["{", namespace, "}loc"]) == elementName x) elements
    lastm =  L.find (\ x -> (fromString $ T.unpack $ T.concat ["{", namespace, "}lastmod"])  == elementName x) elements
    elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes el

isElement :: Node -> Bool
isElement (NodeElement _) = True
isElement _ = False

isContent :: Node -> Bool
isContent (NodeContent _) = True
isContent _ = False

firstMaybe :: [Maybe a] -> Maybe a
firstMaybe ((Just x):xs) = (Just x)
firstMaybe (Nothing:xs) = firstMaybe xs
firstMaybe [] = Nothing

unescapeNews :: Maybe News -> Maybe News
unescapeNews Nothing = Nothing
unescapeNews (Just (News pub genres date title keywords tickers)) = Just $ News pub (fmap unescape genres) date (unescape title) (fmap unescape keywords) (fmap unescape tickers)   

unescape :: Text -> Text
unescape text = T.replace "&amp;" "&" text

unescapeResults :: SitemapResult -> SitemapResult
unescapeResults (UrlSet items) = UrlSet $ fmap (\ x -> x{suNews = unescapeNews $ suNews x}) items
unescapeResults x = x

handleFailedParsing :: MonadIO m => Text -> SomeException -> m (Either SomeException SitemapResult)
handleFailedParsing text e = do
  liftIO $ throw e `catches` [Handler (\ (ex :: UnresolvedEntityException) -> handleUnresolvedEntityException)]
  where
    handleUnresolvedEntityException = do
      let text' = T.replace "&" "&amp;" text
      result' <- parseSitemap text'
      case result' of
        y@(Left _) -> return y
        Right y -> return $ Right $ unescapeResults y

parseSitemap :: MonadIO m => Text -> m (Either SomeException SitemapResult)
parseSitemap text =
  case parseText def $ TL.fromStrict text of
    Left x -> handleFailedParsing text x
    Right x -> do
      let el = documentRoot x
      case elementName el of
        x | x == "{http://www.sitemaps.org/schemas/sitemap/0.9}sitemapindex" -> do
          let elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes  el
          r <- mapM (parseSitemapItem "http://www.sitemaps.org/schemas/sitemap/0.9") elements
          return $ Right $ Sitemap $ catMaybes r
        x | x == "{https://www.sitemaps.org/schemas/sitemap/0.9}sitemapindex" -> do
          let elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes  el
          r <- mapM (parseSitemapItem "https://www.sitemaps.org/schemas/sitemap/0.9") elements
          return $ Right $ Sitemap $ catMaybes r
        x | x == "{http://www.google.com/schemas/sitemap/0.84}sitemapindex" -> do
          let elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes  el
          r <- mapM (parseSitemapItem "http://www.google.com/schemas/sitemap/0.84") elements
          return $ Right $ Sitemap $ catMaybes r
        x | x == "{http://www.sitemaps.org/schemas/sitemap/0.9}indexes" -> do
          let elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes  el
          r <- mapM (parseSitemapItem "http://www.sitemaps.org/schemas/sitemap/0.9") elements
          return $ Right $ Sitemap $ catMaybes r
        x | x == "{http://www.sitemaps.org/schemas/sitemap/0.9}urlset" -> do
          let elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes  el
          r <- mapM (parseUrlItem (Just "http://www.sitemaps.org/schemas/sitemap/0.9")) elements
          return $ Right $ UrlSet $ catMaybes r
        x | x == "{https://www.sitemaps.org/schemas/sitemap/0.9}urlset" -> do
          let elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes  el
          r <- mapM (parseUrlItem (Just "https://www.sitemaps.org/schemas/sitemap/0.9")) elements
          return $ Right $ UrlSet $ catMaybes r
        x | x == "{http://www.google.com/schemas/sitemap/0.9}urlset" -> do
          let elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes  el
          r <- mapM (parseUrlItem (Just "http://www.google.com/schemas/sitemap/0.9")) elements
          return $ Right $ UrlSet $ catMaybes r
        x | x == "{http://www.google.com/schemas/sitemap/0.84}urlset" -> do
          let elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes  el
          r <- mapM (parseUrlItem (Just "http://www.google.com/schemas/sitemap/0.84")) elements
          return $ Right $ UrlSet $ catMaybes r
        x | x == "urlset" -> do
          let elements = fmap (\ (NodeElement x) -> x) $ L.filter isElement $ elementNodes  el
          r <- mapM (parseUrlItem Nothing) elements
          return $ Right $ UrlSet $ catMaybes r
