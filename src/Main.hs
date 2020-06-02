{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Web.Scotty

import qualified Reddit as R
import Reddit.Types.User (Username)
import Reddit.Types.Post (PostListing, Post, PostContent, PostID, content)
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

opts = R.RedditOptions True Nothing R.Anonymous (Just "haskell api test (by /u/thidr0)")

-- todo: add a main page
main :: IO ()
main = scotty 3000 $ do
  get "/" $ html "<h1>Go to /<username></h1>"

  get "/:user" $ do
    user <- param "user"
    posts <- usersPosts user
    html $ mconcat ["<h1>", user, " not found.</h1>"]
    case posts of
      Nothing -> html $ mconcat ["<h1>", user, " not found.</h1>"]
      Just pl -> html $ linksToHtml (S.toList pl)

-- TODO: break this up: get post listings, convert to datatype for display, add to set, display
usersPosts :: TL.Text -> ActionM (Maybe (S.Set TS.Text))
usersPosts user = go (R.Username (TL.toStrict user)) Nothing
  where 
    res :: Maybe PostID -> R.Username -> ActionM (Either (R.APIError R.RedditError) PostListing)
    res Nothing = R.runRedditWith opts . R.getUserPosts' (R.Options Nothing (Just 100))
    res (Just pid) = R.runRedditWith opts . R.getUserPosts' (R.Options (Just (R.After pid)) (Just 100))
    
    go :: Username -> Maybe PostID -> ActionM (Maybe (S.Set TS.Text))
    go username pid = do
      r <- res pid username
      case r of
        Left err -> return Nothing
        Right pl ->
          let a     = R.after pl
              links = getPostLinks $ R.contents pl
            in
            case a of
              Nothing   -> return $ Just links
              Just pid' -> do
                posts <- go username (Just pid')
                case posts of
                  Nothing     -> return $ Just links
                  Just posts' -> return $ Just (links `S.union` posts')

getPostLinks :: [Post] -> S.Set TS.Text
getPostLinks = S.fromList . mapMaybe (linkText . content)

isPicture :: TS.Text -> Bool
isPicture t = TS.unpack t =~ ("\\.(jpe?g|png|tiff|gif)$" :: String)

linkText :: PostContent -> Maybe TS.Text
linkText (R.Link t) = if isPicture t then Just t else Nothing
linkText _ = Nothing

linksToHtml :: [TS.Text] -> TL.Text
linksToHtml ts = TL.pack $ [r|<html>
<head><title>Gallery</title>
<style type="text/css">
img {width:100%}
</style>
</head>
<body>|] ++ links ++ [r|</body></html>|]
  where links = unwords $ linkPrintf <$> ts
        linkPrintf s = printf "<a href = \"%s\"><img src=\"%s\" /></a>\n" s s 