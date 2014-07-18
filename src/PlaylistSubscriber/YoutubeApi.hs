{-# LANGUAGE ScopedTypeVariables #-}
module PlaylistSubscriber.YoutubeApi where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson as A
import Data.List
import Data.Maybe
import Network.HTTP.Conduit as C
import Network.Connection(TLSSettings(TLSSettingsSimple))
import System.Directory
import System.FilePath.Posix ((</>))
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

newtype PlaylistId = PlaylistId { unPlaylistId :: String } deriving (Eq, Show)
newtype VideoId = VideoId { unVideoId :: String } deriving (Eq, Show)
newtype PageToken = PageToken { unPageToken :: String } deriving (Eq, Show)

data Video = Video {
	idOfVideo :: VideoId,
	numberOfVideo :: Integer,
	titleOfVideo :: String
	} deriving Show
data PlaylistPart = PlaylistPart [Video] (Maybe PageToken)
data Playlist = Playlist {
	idOfPlaylist :: PlaylistId,
	titleOfPlaylist :: String,
	ownerOfPlaylist :: String,
	videosOfPlaylist :: [Video]} deriving Show
data FewVideos = FewVideos [Video] (Maybe PageToken)

data Config = Config { apiKey :: String, manager :: C.Manager }

mySimpleHttp :: Config -> String -> IO L.ByteString
mySimpleHttp cfg url = do
	baseRequest <- C.parseUrl url
	let request = baseRequest --{responseTimeout = Just 1500000}
	res <- C.httpLbs request $ manager cfg
	return $ C.responseBody res

emptyPlaylist :: Playlist
emptyPlaylist = Playlist (PlaylistId "0") "<unknown playlist>" "<unknown channel>" []

insertPageToken :: Maybe PageToken -> String
insertPageToken maybeToken = maybe "" (("&pageToken=" ++) . unPageToken) maybeToken

getConfig :: IO Config
getConfig = do
	home <- getHomeDirectory
	let apiKeyFile = home </> ".youtubeApiKey"
	exists <- doesFileExist apiKeyFile
	when (not exists) $ do
		putStrLn $ "File " ++ apiKeyFile ++ " does not exists"
		putStrLn "Go to https://developers.google.com/youtube/registering_an_application and acquire an API key"
		error "No API key found"
	apiKeyFileContents <- readFile apiKeyFile
	let ak = (head $ filter (not . null) $ lines apiKeyFileContents)
	mgr <- newManager $ C.mkManagerSettings (TLSSettingsSimple True False False) Nothing
	return $ Config ak mgr

playlistItemsApiUrl :: Config -> PlaylistId -> Maybe PageToken -> String
playlistItemsApiUrl cfg playlistId pageToken =
	"https://www.googleapis.com/youtube/v3/playlistItems?part=snippet&playlistId="++
	unPlaylistId playlistId ++
	insertPageToken pageToken ++
	"&maxResults=50&key=" ++ apiKey cfg

playlistApiUrl :: Config -> PlaylistId -> String
playlistApiUrl cfg playlistId =
	"https://www.googleapis.com/youtube/v3/playlists?part=snippet&id="++
	unPlaylistId playlistId ++
	"&key=" ++ apiKey cfg

getPlaylistPart :: Config -> PlaylistId -> Maybe PageToken -> IO (Maybe PlaylistPart)
getPlaylistPart cfg playlistId pageToken = do
	flip catch (\(_::SomeException) -> do
		putStrLn (
			"Failed to get " ++
			unPlaylistId playlistId ++
			". Is it down? https://www.youtube.com/playlist?list=" ++
			unPlaylistId playlistId )
		return Nothing
		) $ do
			response <- mySimpleHttp cfg $
				playlistItemsApiUrl cfg playlistId pageToken :: IO L.ByteString
			case A.decode response of
				Just o -> do
					let snippets = jpathArray ["items", "snippet"] o :: [A.Value]
					let videos = catMaybes $ map (\s ->
						let
							maybeId = jsonGetString ["resourceId", "videoId"] s
							maybePosition = jsonGetInteger ["position"] s
							maybeTitle = jsonGetString ["title"] s
						in
							Video <$>
							(VideoId <$> maybeId) <*>
							((+1) <$> maybePosition) <*>
							maybeTitle
						) snippets
					return $ Just $ PlaylistPart videos (
						PageToken <$> jsonGetString ["nextPageToken"] o
						)
				_ -> return Nothing

getPlaylistTitleAndOwner :: Config -> PlaylistId -> IO (String, String)
getPlaylistTitleAndOwner cfg playlistId = do
	flip catch (\(_::SomeException) -> return ("<unknown playlist>", "<unknown channel>")) $ do
		response <- mySimpleHttp cfg $
			playlistApiUrl cfg playlistId :: IO L.ByteString
		case A.decode response of
			Just o ->
				let
					maybeTitle = jsonGetString ["items", "snippet", "title"] o
					maybeOwner = jsonGetString ["items", "snippet", "channelTitle"] o
				in
					return (fromMaybe "<unknown playlist>" maybeTitle, fromMaybe "<unknown channel>" maybeOwner)
			_ -> return ("<unknown playlist>", "<unknown channel>")


getPlaylist :: Config -> PlaylistId -> IO (Maybe Playlist)
getPlaylist cfg playlistId = do
	putStrLn $ "Loading playlist " ++ unPlaylistId playlistId
	maybePage0 <- getPlaylistPart cfg playlistId Nothing
	case maybePage0 of
		Nothing -> return Nothing
		Just (PlaylistPart videos token) -> do
			moreVideos <- getMore token
			(title, owner) <-
				getPlaylistTitleAndOwner cfg playlistId
			return $ Just $ Playlist playlistId title owner $ concat $ videos:moreVideos
	where
		getMore :: Maybe PageToken -> IO [[Video]]
		getMore Nothing = return []
		getMore (Just token) = do
			maybeNextPage <- getPlaylistPart cfg playlistId (Just token)
			case maybeNextPage of
				Nothing -> return []
				Just (PlaylistPart videos nextToken) -> (videos:) <$> getMore nextToken


jpath :: [String] -> A.Value -> Maybe A.Value
jpath [] o = Just o
jpath (x:xs) (Object o) = H.lookup (T.pack x) o >>= jpath xs
jpath p (Array a) = jpath p $ V.head a
jpath _ _ = Nothing

jpathArray :: [String] -> A.Value -> [A.Value]
jpathArray p (Array a) = V.toList a >>= jpathArray p
jpathArray [] o = [o]
jpathArray (x:xs) (Object o) = case H.lookup (T.pack x) o of
	Nothing -> []
	Just o1 -> jpathArray xs o1
jpathArray _ _ = []

jsonStringify :: A.Value -> String
jsonStringify (A.String t) = T.unpack t
jsonStringify (A.Number n) = show n
jsonStringify (A.Bool True) = "true"
jsonStringify (A.Bool False) = "false"
jsonStringify _ = "?"

jsonAsInteger :: A.Value -> Integer
jsonAsInteger (A.String t) = read $ T.unpack t
jsonAsInteger (A.Number n) = round n
jsonAsInteger (A.Bool True) = 1
jsonAsInteger (A.Bool False) = 0
jsonAsInteger _ = 0


jsonGetString :: [String] -> A.Value -> Maybe String
jsonGetString p j = jsonStringify <$> jpath p j

jsonGetInteger :: [String] -> A.Value -> Maybe Integer
jsonGetInteger p j = jsonAsInteger <$> jpath p j

jsonGetStrings :: [String] -> A.Value -> [String]
jsonGetStrings p j = jsonStringify <$> jpathArray p j

playListIdFromString :: String -> PlaylistId
playListIdFromString s
	| isPrefixOf "https://www.youtube.com/playlist?list=" s = playListIdFromString $ drop 38 s
	| isPrefixOf "http://www.youtube.com/playlist?list=" s = playListIdFromString $ drop 37 s
	| isPrefixOf "PL" s && length s > 16 = PlaylistId s
	| isPrefixOf "FL" s && length s > 16 = PlaylistId s
	| otherwise = PlaylistId ("PL" ++ s)

videoUrl :: Video -> String
videoUrl v = "http://www.youtube.com/watch?v=" ++ unVideoId (idOfVideo v)

playlistUrl :: Playlist -> String
playlistUrl p = "http://www.youtube.com/playlist?list=" ++ unPlaylistId (idOfPlaylist p)