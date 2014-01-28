joinString :: String -> [String] -> String
joinString _ [] = ""
joinString sep [ value ] = value
joinString sep (value:values) = value ++ sep ++ joinString sep values

-- let p = Plug { scheme = "http", username = Nothing, password = Nothing, hostname = "example.com", port = 80, path = Nothing, query = Nothing, fragment = Nothing }
-- p `withScheme` "https" `withPort` 443 `at` "x" `at` "y" `with` ("a", Just "b") `withFragment` "foo"


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

-- TODO (steveb): comparison needs to be case-insensitive
isDefaultPort :: String -> Int -> Bool
isDefaultPort "http" 80 = True
isDefaultPort "https" 443 = True
isDefaultPort "ftp" 21 = True
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

withParams :: Plug -> [(String, Maybe String)] -> Plug
withParams plug@Plug { query = Nothing } kvs = plug { query = Just kvs }
withParams plug@Plug { query = Just kvs' } kvs = plug { query = Just (kvs' ++ kvs) }

-- withQuery
-- withoutQuery
-- getParam
-- getParams

-- without key (Plug scheme hostname port path query fragment) = Plug scheme hostname port path query fragment

withFragment :: Plug -> String -> Plug
withFragment plug fragment' = plug { fragment = Just fragment' }

withoutFragment :: Plug -> Plug
withoutFragment plug@Plug { fragment = Nothing } = plug
withoutFragment plug = plug { fragment = Nothing }

-- TODO: missing implementation for encoding
encodeUserInfo :: String -> String
encodeUserInfo text = text

-- TODO: missing implementation for encoding
encodeSegment :: String -> String
encodeSegment text = text

-- TODO: missing implementation for encoding
encodeQuery :: String -> String
encodeQuery text = text

-- TODO: missing implementation for encoding
encodeFragment :: String -> String
encodeFragment text = text

getUserInfo :: Plug -> String
getUserInfo Plug { username = Nothing, password = Nothing } = ""
getUserInfo Plug { username = Nothing, password = Just password' } = ':' : encodeUserInfo password' ++ "@"
getUserInfo Plug { username = Just username', password = Nothing } = encodeUserInfo username' ++ "@"
getUserInfo Plug { username = Just username', password = Just password' } = encodeUserInfo username' ++ ":" ++ encodeUserInfo password' ++ "@"

getHost :: Plug -> String
getHost plug = (hostname plug) ++ (if usesDefaultPort plug then "" else ':' : show (port plug) )

getPath :: Plug -> String
getPath Plug { path = Nothing } = ""
getPath Plug { path = Just (segments', True) } = '/' : joinString "/" (map encodeSegment segments') ++ "/"
getPath Plug { path = Just (segments', False) } = '/' : joinString "/" (map encodeSegment segments')

getQuery :: Plug -> String
getQuery Plug { query = Nothing } = ""
getQuery Plug { query = Just kvs } = '?' : joinString "&" (map showQueryKeyValue kvs)
    where showQueryKeyValue (k, Nothing) = encodeQuery k
          showQueryKeyValue (k, (Just v)) = encodeQuery k ++ "=" ++ encodeQuery v

getFragment :: Plug -> String
getFragment Plug { fragment = Nothing } = ""
getFragment Plug { fragment = Just fragment' } = '#' : encodeFragment fragment'

instance Show Plug where
    show p = scheme p ++ "://" ++ getUserInfo p ++ getHost p ++ getPath p ++ getQuery p ++ getFragment p
