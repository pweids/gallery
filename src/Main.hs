{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Reddit as R
import Reddit.Types.User (Username)
import Reddit.Types.Post (PostListing, Post, PostContent, PostID, content)
import Reddit.Types.Options (Options, PaginationOption)

import Text.Printf
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Text.RawString.QQ

opts = R.RedditOptions True Nothing R.Anonymous (Just "haskell api test (by /u/thidr0)")

main :: IO ()
main = do
  putStrLn "Enter user's name:"
  user <- getLine
  posts <- usersPosts user
  case posts of
    Nothing -> do
      putStrLn "user not found"
      return ()
    Just pl -> let fp = user ++ ".html" in
      do
        putStrLn $ "html written to " ++ fp
        writeLinks fp (S.toList pl)

writeLinks :: FilePath -> [T.Text] -> IO ()
writeLinks fp ls = writeFile fp $ linksToHtml ls

usersPosts :: String -> IO (Maybe (S.Set T.Text))
usersPosts user = go (R.Username $ T.pack user) Nothing
  where 
    res :: Maybe PostID -> R.Username -> IO (Either (R.APIError R.RedditError) PostListing)
    res Nothing = R.runRedditWith opts . R.getUserPosts' (R.Options Nothing (Just 100))
    res (Just pid) = R.runRedditWith opts . R.getUserPosts' (R.Options (Just (R.After pid)) (Just 100))
    
    go :: Username -> Maybe PostID -> IO (Maybe (S.Set T.Text))
    go username pid = do
      r <- res pid username
      case r of
        Left err -> return Nothing
        Right pl ->
          let a = R.after pl
              links = (getPostLinks $ R.contents pl) 
            in
            case a of
              Nothing -> return $ Just links
              Just pid' -> do
                posts <- go username (Just pid')
                case posts of
                  Nothing -> return $ Just links
                  Just posts' -> return $ Just (links `S.union` posts')

getPostLinks :: [Post] -> S.Set T.Text
getPostLinks = S.fromList . mapMaybe (linkText . content)

linkText :: PostContent -> Maybe T.Text
linkText (R.Link t) = Just t
linkText _ = Nothing

linksToHtml :: [T.Text] -> String
linksToHtml ts = [r|<html>
<head><title>Gallery</title>
<style type="text/css">
img {width:100%}
</style>
</head>
<body>|] ++ links ++ [r|</body></html>|]
  where links = unwords $ linkPrintf <$> map T.unpack ts
        linkPrintf s = printf "<a href = \"%s\"><img src=\"%s\" /></a>\n" s s 