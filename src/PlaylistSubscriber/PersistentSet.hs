module PlaylistSubscriber.PersistentSet where

import System.Directory

data PersistentSet a = PersistentSet {
	persistentSetPath :: FilePath,
	persistentSetSerialization :: (a->String),
	persistentSetDeserialization :: (String->a)
	}


readPersistentSet :: PersistentSet a -> IO [a]
readPersistentSet s = do
	let p = persistentSetPath s
	exists <- doesFileExist p
	if exists
	then do
		contents <- readFile p
		return $ map (persistentSetDeserialization s) $ filter (/="") $ lines contents
	else return []



savePersistentSet :: PersistentSet a -> [a] -> IO ()
savePersistentSet s d = 
	writeFile (persistentSetPath s) (unlines $ map (persistentSetSerialization s) d)