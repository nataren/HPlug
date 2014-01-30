import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toUpper)
import Data.List (intercalate)
import Network.URI (escapeURIString)

join :: [a] -> [[a]] -> [a]
join = intercalate

equalsIgnoreCase :: String -> String -> Bool
equalsIgnoreCase a b = map toUpper a == map toUpper b

-- let p = Plug { scheme = "http", username = Nothing, password = Nothing, hostname = "example.com", port = 80, path = Nothing, query = Nothing, fragment = Nothing }
-- p `withScheme` "https" withCredentials ("john", "pwd") `withPort` 443 `at` "x" `at` "y" `with` ("a", Just "b") `withFragment` "foo"

data Plug = Plug {
    scheme          :: String,
    username        :: Maybe String,
    password        :: Maybe String,
    hostname        :: String,
    port            :: Int,
    path            :: Maybe ([String], Bool),
    query           :: Maybe [(String, Maybe String)],
    fragment        :: Maybe String
} deriving (Eq)

isDefaultPort :: String -> Int -> Bool
isDefaultPort scheme' 80 = scheme' `equalsIgnoreCase` "http"
isDefaultPort scheme' 443 = scheme' `equalsIgnoreCase` "https"
isDefaultPort scheme' 21 = scheme' `equalsIgnoreCase` "ftp"
isDefaultPort _ _ = False

usesDefaultPort :: Plug -> Bool
usesDefaultPort plug = isDefaultPort (scheme plug) (port plug)

withScheme :: Plug -> String -> Plug
withScheme plug scheme' = plug { scheme = scheme' }

withCredentials :: Plug -> (String, String) -> Plug
withCredentials plug (username', password') = plug { username = Just username', password = Just password' } 

withoutCredentials :: Plug -> Plug
withoutCredentials plug@Plug { username = Nothing, password = Nothing } = plug
withoutCredentials plug = plug { username = Nothing, password = Nothing }

withHostname :: Plug -> String -> Plug
withHostname plug hostname' = plug { hostname = hostname' }

withPort :: Plug -> Int -> Plug
withPort plug port' = plug { port = port' }

withTrailingSlash :: Plug -> Plug
withTrailingSlash plug@Plug { path = Nothing } = plug { path = Just ([], True) }
withTrailingSlash plug@Plug { path = Just (segments', True) } = plug
withTrailingSlash plug@Plug { path = Just (segments', False) } = plug { path = Just (segments', True) }

withoutTrailingSlash :: Plug -> Plug
withoutTrailingSlash plug@Plug { path = Nothing } = plug
withoutTrailingSlash plug@Plug { path = Just (segments', True) } = plug { path = Just (segments', False) }
withoutTrailingSlash plug@Plug { path = Just (segments', False ) } = plug

at :: Plug -> String -> Plug
at plug@Plug { path = Nothing } segment = plug { path = Just ([ segment ], False)}
at plug@Plug { path = Just (segments', trailingSlash') } segment = plug { path = Just (segments' ++ [ segment ], trailingSlash')}

with :: Plug -> (String, Maybe String) -> Plug
with plug@Plug { query = Nothing } kv = plug { query = Just [ kv ]}
with plug@Plug { query = Just kvs } kv = plug { query = Just (kvs ++ [ kv ])}

without :: Plug -> String -> Plug
without plug@Plug { query = Nothing } _ = plug
without plug@Plug { query = Just queryParams } key = plug { query = if null queryParams' then Nothing else Just queryParams' }
    where matchesKey (key', _) = not (key `equalsIgnoreCase` key')
          queryParams' = filter matchesKey queryParams

withParams :: Plug -> [(String, Maybe String)] -> Plug
withParams plug@Plug { query = Nothing } kvs = plug { query = Just kvs }
withParams plug@Plug { query = Just kvs' } kvs = plug { query = Just (kvs' ++ kvs) }

-- TODO
-- withQuery :: Plug -> String -> Plug

withoutQuery :: Plug -> Plug
withoutQuery plug = plug { query = Nothing }

getParam :: Plug -> String -> Maybe (Maybe String)
getParam plug@Plug { query = Nothing } _ = Nothing
getParam plug@Plug { query = Just queryParams } key = if null matchedParams then Nothing else Just $ head matchedParams
    where matchedParams = plug `getParams` key

getParams :: Plug -> String -> [ Maybe String ]
getParams plug@Plug { query = Nothing } _ = [ ]
getParams plug@Plug { query = Just queryParams } key = map selectValue $ filter matchesKey queryParams
    where matchesKey (key', _) = key `equalsIgnoreCase` key'
          selectValue (_, value) = value

withFragment :: Plug -> String -> Plug
withFragment plug fragment' = plug { fragment = Just fragment' }

withoutFragment :: Plug -> Plug
withoutFragment plug@Plug { fragment = Nothing } = plug
withoutFragment plug = plug { fragment = Nothing }

data EncodingLevel = UserInfo | Segment | Query | Fragment
    deriving (Ord, Eq)

isValidUriChar :: EncodingLevel -> Char -> Bool
isValidUriChar l c
    | isAsciiLower c || isAsciiUpper c || isDigit c = True
    | c `elem` "'()*-._!" = True
    | l >= Fragment && c == '#' = True
    | l >= Query && (c `elem` "/:-$,;|") = True
    | l >= Segment && (c `elem` "@^") = True
    | l == UserInfo && (c `elem` "&=") = True
    | otherwise = False

encodeUserInfo :: String -> String
encodeUserInfo = escapeURIString (isValidUriChar UserInfo)

encodeSegment :: String -> String
encodeSegment = escapeURIString (isValidUriChar Segment)

encodeQuery :: String -> String
encodeQuery = escapeURIString (isValidUriChar Query)

encodeFragment :: String -> String
encodeFragment = escapeURIString (isValidUriChar Fragment)

getUserInfo :: Plug -> String
getUserInfo Plug { username = Nothing, password = Nothing } = ""
getUserInfo Plug { username = Nothing, password = Just password' } = ':' : encodeUserInfo password' ++ "@"
getUserInfo Plug { username = Just username', password = Nothing } = encodeUserInfo username' ++ "@"
getUserInfo Plug { username = Just username', password = Just password' } = encodeUserInfo username' ++ ":" ++ encodeUserInfo password' ++ "@"

getHost :: Plug -> String
getHost plug = hostname plug ++ (if usesDefaultPort plug then "" else ':' : show (port plug) )

getPath :: Plug -> String
getPath Plug { path = Nothing } = ""
getPath Plug { path = Just (segments', True) } = '/' : join "/" (map encodeSegment segments') ++ "/"
getPath Plug { path = Just (segments', False) } = '/' : join "/" (map encodeSegment segments')

getQuery :: Plug -> String
getQuery Plug { query = Nothing } = ""
getQuery Plug { query = Just kvs } = '?' : join "&" (map showQueryKeyValue kvs)
    where showQueryKeyValue (k, Nothing) = encodeQuery k
          showQueryKeyValue (k, Just v) = encodeQuery k ++ "=" ++ encodeQuery v

getFragment :: Plug -> String
getFragment Plug { fragment = Nothing } = ""
getFragment Plug { fragment = Just fragment' } = '#' : encodeFragment fragment'

instance Show Plug where
    show p = scheme p ++ "://" ++ getUserInfo p ++ getHost p ++ getPath p ++ getQuery p ++ getFragment p
