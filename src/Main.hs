{-# LANGUAGE TupleSections #-}
module Main where

import PlaylistSubscriber.YoutubeApi as Y
import PlaylistSubscriber.PersistentSet
import Data.Maybe
import Control.Applicative
import Control.Monad
import System.Directory
import System.FilePath.Posix

videoIdSet :: FilePath -> PersistentSet Y.VideoId
videoIdSet f = PersistentSet f Y.unVideoId Y.VideoId

playlistIdSet :: FilePath -> PersistentSet Y.PlaylistId
playlistIdSet f = PersistentSet f Y.unPlaylistId Y.playListIdFromString

getUnseenVideos :: [Y.VideoId] -> [Y.PlaylistId] -> IO [(Y.Playlist, [Y.Video])]
getUnseenVideos seenVideoIds playlistIds = do
	cfg <- getConfig
	playlists <- catMaybes <$> mapM (Y.getPlaylist cfg) playlistIds
	let taggedVideos = map (\p -> (p, filter (\v -> not $ idOfVideo v `elem` seenVideoIds) $ Y.videosOfPlaylist p)) playlists
	return $ filter (not . null . snd) taggedVideos

yesOrNo :: String -> IO Bool
yesOrNo query = do
	putStrLn query
	answer <- getLine 
	case answer of
		"y" -> return True
		"Y" -> return True
		"yes" -> return True
		"YES" -> return True
		"n" -> return False
		"N" -> return False
		"no" -> return False
		"NO" -> return False
		_ -> putStrLn "Please answer yes or no." >> yesOrNo query

main :: IO()
main = do
	home <- getHomeDirectory
	let subscriptionsFile = home </> ".playlistSubscriptions"
	exists <- doesFileExist subscriptionsFile
	when (not exists) $ do
		putStrLn $ "File " ++ subscriptionsFile ++ " does not exists"
		error "No playlist subscriptions"
	myPlayLists <- readPersistentSet $ playlistIdSet subscriptionsFile
	putStrLn $ "Subscribed to " ++ show(length myPlayLists) ++ " playlists"
	let seenVideoIdSet = videoIdSet $ home </> ".seen"
	seenVideoIds <- readPersistentSet seenVideoIdSet
	taggedVideos <- getUnseenVideos seenVideoIds myPlayLists
	forM taggedVideos $ \(p,videos) -> do
		putStrLn $ Y.titleOfPlaylist p ++ " by " ++ Y.ownerOfPlaylist p
		putStrLn $ Y.playlistUrl p
		forM videos $ \v -> do
			putStrLn $ show (Y.numberOfVideo v) ++ " " ++ Y.titleOfVideo v
			putStrLn $ "    " ++ Y.videoUrl v
	if (not $ null taggedVideos) 
	then do
		y <- yesOrNo "Mark as seen?"
		when y $ do
			putStrLn $ "Marking " ++ show (sum $ map (length . snd) taggedVideos) ++ " videos as seen"
			savePersistentSet seenVideoIdSet (seenVideoIds ++  concatMap (fmap idOfVideo . snd) taggedVideos)
	else putStrLn "No new videos"
	return ()