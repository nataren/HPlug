module MindTouch.Http(
  defaultPlug,
  Plug,
  Response,
  withScheme,
  withHostname,
  withPort,
  withTrailingSlash,
  withoutTrailingSlash,
  with,
  at,
  usesDefaultPort,
  withCredentials,
  get,
  testPlug) where
import qualified Network.HTTP.Conduit as Conduit
import Data.Conduit
import qualified Data.ByteString.Lazy as L
import Network (withSocketsDo)
import Data.List (intersperse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import Control.Monad.IO.Class (liftIO)

data Plug = Plug {
  scheme        :: String,
  hostname      :: String,
  port          :: Int,
  path          :: [String],
  query         :: [(String, Maybe String)],
  fragment      :: Maybe String,
  trailingslash :: Bool,
  username      :: Maybe String,
  password      :: Maybe String
} deriving(Eq)

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

-- TODO: missing implementation for encoding
encodeSegment :: String -> String
encodeSegment text = text

-- TODO: missing implementation for encoding
encodeQuery :: String -> String
encodeQuery text = text

-- TODO: missing implementation for encoding
encodeFragment :: String -> String
encodeFragment text = text

getHost :: Plug -> String
getHost plug = hostname plug ++ (if usesDefaultPort plug then [] else ":" ++ show (port plug))

getPath :: Plug -> String
getPath plug = '/' : join "/" (map encodeSegment (path plug)) ++ (if trailingslash plug then "/" else "")

getQuery :: Plug -> String
getQuery plug = join "&" (map tostring (query plug))
  where
    tostring query' = case query' of
      (name, Nothing) -> encodeQuery name
      (name, Just value) -> encodeQuery name ++ "=" ++ encodeQuery value

getFragment :: Plug -> String
getFragment plug = case fragment plug of
  Nothing -> ""
  Just fragment' -> '#' : encodeFragment fragment'
  
instance Show Plug where
  show p = scheme p ++ "://" ++ getHost p ++ getPath p ++ "?" ++ getQuery p ++ getFragment p


data Status = OK
data Response = Response {
  status      :: Status
}
  
defaultPlug :: Plug
defaultPlug     = Plug {
  scheme        = "http",
  hostname      = "localhost",
  port          = 80,
  path          = [],
  query         = [],
  fragment      = Nothing,
  trailingslash = False,
  username      = Nothing,
  password      = Nothing
}

withScheme :: Plug -> String -> Plug
withScheme plug scheme' = plug { scheme = scheme' }
  
withHostname :: Plug -> String -> Plug
withHostname plug hostname' = plug { hostname = hostname' }

withPort :: Plug -> Int -> Plug
withPort plug port' = plug { port = port' }

withTrailingSlash :: Plug -> Plug
withTrailingSlash plug = plug { trailingslash = True }

withoutTrailingSlash :: Plug -> Plug
withoutTrailingSlash plug = plug { trailingslash = False }

withCredentials :: Plug -> (String, String) -> Plug
withCredentials plug (username', password') = plug {
  username = Just username',
  password = Just password'
}

at :: Plug -> String -> Plug
at plug segment = plug { path = (path plug) ++ [segment] }

with :: Show a => Plug -> (String, Maybe a) -> Plug
with plug kvp =
  let (key, value) = kvp in
    plug { query = (query plug) ++ [(key, convert value)] }
  where
    convert value = case value of
      Nothing -> Nothing
      Just v -> Just (show v) 
    
-- TODO (steveb): comparison needs to be case-insensitive
isDefaultPort :: String -> Int -> Bool
isDefaultPort "http" 80 = True
isDefaultPort "https" 443 = True
isDefaultPort "ftp" 21 = True
isDefaultPort _ _ = False

usesDefaultPort :: Plug -> Bool
usesDefaultPort plug = isDefaultPort (scheme plug) (port plug)

getAuthRequest :: Maybe String -> Maybe String -> Conduit.Request -> Conduit.Request
getAuthRequest username' password' request' =
  case (username', password') of
    (Just u, Just p) -> Conduit.applyBasicAuth (BC.pack u) (BC.pack p) request'
    (_, _) -> request'

get :: Plug -> IO (Conduit.Response (ResumableSource (ResourceT IO) BC.ByteString))
get plug = withSocketsDo $ Conduit.withManager $ \manager -> do
  request <- Conduit.parseUrl (show plug)
  let request' = getAuthRequest (username plug) (password plug) request
  response <- Conduit.http request' manager
  return response
  
-- post :: Plug -> Response
-- put :: Plug -> Response


testPlug :: Plug
testPlug =
  defaultPlug
  `withHostname` "joelsite.mindtouch.us"
  `withPort` 80
  `withScheme` "http"
  `at` "a"
  `at` "b"
  `at` "c"
  `with` ("pageid", Just 1 :: Maybe Int)
  `with` ("recursive", Just True)
  `with` ("emptyparam", Nothing :: Maybe String)
  `withCredentials` ("cesarn", "fooboo")