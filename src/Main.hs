{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Web.Scotty

import qualified Reddit as R
import Reddit.Types.User (Username)
import Reddit.Types.Post (PostListing, Post, PostContent, PostID, content, permalink, title, created)
import Reddit.Types.Options (Options, PaginationOption)

import Text.Printf
import Data.String (IsString(..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text as TS
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Text.RawString.QQ
import Data.Monoid (mconcat)
import Text.Regex.Posix
import Data.Time.Clock (UTCTime, utctDay)
import Control.Monad.IO.Class (liftIO)

opts = R.RedditOptions True Nothing R.Anonymous (Just "haskell api test (by /u/thidr0)")

data Image = 
  Image { imgUrl  :: TS.Text
        , postUrl :: TS.Text
        , postTitle   :: TS.Text
        , date :: UTCTime }

-- todo: add a main page
main :: IO ()
main = scotty 3000 $ do
  get "/" $ html [r|<html><head>
<script>
    function process()
    {
        var url = "/" + document.getElementById("username").value;
        location.href = url;
        return false;
    }
</script>
<title>Reddit Gallery</title></head><body>
<form onSubmit="return process();">
    Username: <input type="text" name="username" id="username">
    <input type="submit" value="go">
</form>
</body>
|]

  get "/:user" $ do
    user <- param "user"
    imgs <- usersImgs user
    html $ mconcat ["<h1>", user, " not found.</h1>"]
    case imgs of
      Nothing -> html $ mconcat ["<h1>", user, " not found.</h1>"]
      Just is -> html $ linksToHtml user is


usersImgs :: TL.Text -> ActionM (Maybe [Image])
usersImgs user = go (R.Username (TL.toStrict user)) Nothing
  where 
    go :: Username -> Maybe PostID -> ActionM (Maybe [Image])
    go username pid = do
      r <- userPostListing username pid
      case r of
        Left err -> return Nothing
        Right pl ->
          let a     = R.after pl
              links = getPostImages $ R.contents pl
            in
            case a of
              Nothing   -> return $ Just links
              Just pid' -> do
                posts <- go username (Just pid')
                case posts of
                  Nothing     -> return $ Just links
                  Just posts' -> return $ Just (links ++ posts')

userPostListing :: R.Username -> Maybe PostID -> ActionM (Either (R.APIError R.RedditError) PostListing)
userPostListing username mpid = R.runRedditWith opts $ gup mpid
  where gup Nothing    = R.getUserPosts' (R.Options Nothing (Just 100)) username
        gup (Just pid) = R.getUserPosts' (R.Options (Just (R.After pid)) (Just 100)) username

getPostImages :: [Post] -> [Image]
getPostImages = filterDups imgUrl . mapMaybe packageImage

packageImage :: Post -> Maybe Image
packageImage pl = let link = linkText $ content pl in
  case link of
    Nothing -> Nothing
    (Just l) -> Just $ Image l (permalink pl) (title pl) (created pl)

linkText :: PostContent -> Maybe TS.Text
linkText (R.Link t) = if isPicture t then Just t else Nothing
linkText _ = Nothing    

isPicture :: TS.Text -> Bool
isPicture t = TS.unpack t =~ ("\\.(jpe?g|png|tiff|gif)$" :: String)

linksToHtml :: TL.Text -> [Image] -> TL.Text
linksToHtml user ts = TL.pack $ [r|<html>
<head><title>Gallery</title>
<style type="text/css">
img {max-width:100%; height:auto}
</style>
</head>
<body><h1>|] ++ TL.unpack user ++ "'s Gallery (" ++ (show $ length ts) ++ " images)" 
  ++ links ++ [r|</body></html>|]
  where links = unwords $ linkPrintf <$> ts
        linkPrintf i = printf 
                        "<h2>%s (%s)</h2><a href = \"https://www.reddit.com%s\"><img src=\"%s\" /></a>\n" 
                         (postTitle i) (show $ utctDay $ date i) (postUrl i) (imgUrl i)

filterDups :: Eq b => (a -> b) -> [a] -> [a]
filterDups _ [] = []
filterDups _ [x] = [x]
filterDups f (x:xs) = x : filterDups f (filter (\y -> f x /= f y) xs)