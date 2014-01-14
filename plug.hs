joinString :: String -> [String] -> String
joinString _ [] = ""
joinString sep [ value ] = value
joinString sep (value:values) = value ++ sep ++ joinString sep values

-- let p = Plug "http" "example.com" 80 True Nothing Nothing Nothing
-- p `withScheme` "https" `withPort` 443 `at` "x" `at` "y" `with` ("a", Just "b") `withFragment` "foo"


data Plug = Plug {
    scheme          :: String,
    hostname        :: String,
    port            :: Int,
    usesDefaultPort :: Bool,
    path            :: Maybe ([String], Bool),
    query           :: Maybe [(String, Maybe String)],
    fragment        :: Maybe String
} deriving (Eq)

--(->) :: Plug -> (Plug -> Plug) -> Plug
--(->) p f = f p

-- (Plug scheme hostname port usesDefaultPort path query fragment) = Plug scheme hostname port usesDefaultPort path query fragment

-- TODO (steveb): comparison needs to be case-insensitive
isDefaultPort :: String -> Int -> Bool
isDefaultPort "http" 80 = True
isDefaultPort "https" 443 = True
isDefaultPort "ftp" 21 = True
isDefaultPort _ _ = False

withScheme :: Plug -> String -> Plug
withScheme (Plug _ hostname port _ path query fragment) scheme = Plug scheme hostname port (isDefaultPort scheme port) path query fragment

withHostname :: Plug -> String -> Plug
withHostname (Plug scheme _ port usesDefaultPort path query fragment) hostname = Plug scheme hostname port usesDefaultPort path query fragment

withPort :: Plug -> Int -> Plug
withPort (Plug scheme hostname _ _ path query fragment) port = Plug scheme hostname port (isDefaultPort scheme port) path query fragment

at :: Plug -> String -> Plug
at (Plug scheme hostname port usesDefaultPort Nothing query fragment) segment = 
    Plug scheme hostname port usesDefaultPort (Just ([ segment ], False)) query fragment
at (Plug scheme hostname port usesDefaultPort (Just (segments, trailingSlash)) query fragment) segment = 
    Plug scheme hostname port usesDefaultPort (Just (segments ++ [ segment ], trailingSlash)) query fragment

withTrailingSlash :: Plug -> Plug
withTrailingSlash (Plug scheme hostname port usesDefaultPort Nothing query fragment) = Plug scheme hostname port usesDefaultPort (Just ([], True)) query fragment
withTrailingSlash (Plug scheme hostname port usesDefaultPort (Just (segments, _)) query fragment) = Plug scheme hostname port usesDefaultPort (Just (segments, True)) query fragment

withoutTrailingSlash :: Plug -> Plug
withoutTrailingSlash (Plug scheme hostname port usesDefaultPort (Just (segments, True)) query fragment) = Plug scheme hostname port usesDefaultPort (Just (segments, False)) query fragment
withoutTrailingSlash p = p


with :: Plug -> (String, Maybe String) -> Plug
with (Plug scheme hostname port usesDefaultPort path Nothing fragment) kv = 
    Plug scheme hostname port usesDefaultPort path (Just [ kv ]) fragment
with (Plug scheme hostname port usesDefaultPort path (Just kvs) fragment) kv = 
    Plug scheme hostname port usesDefaultPort path (Just (kvs ++ [ kv ])) fragment

withParams :: Plug -> [(String, Maybe String)] -> Plug
withParams (Plug scheme hostname port usesDefaultPort path Nothing fragment) kvs = 
    Plug scheme hostname port usesDefaultPort path (Just kvs) fragment
withParams (Plug scheme hostname port usesDefaultPort path (Just kvs1) fragment) kvs2 = 
    Plug scheme hostname port usesDefaultPort path (Just (kvs1 ++ kvs2)) fragment

-- withQuery
-- withoutQuery
-- getParam
-- getParams

-- without key (Plug scheme hostname port usesDefaultPort path query fragment) = Plug scheme hostname port usesDefaultPort path query fragment

withFragment :: Plug -> String -> Plug
withFragment (Plug scheme hostname port usesDefaultPort path query _) fragment = Plug scheme hostname port usesDefaultPort path query (Just fragment)

withoutFragment :: Plug -> Plug
withoutFragment (Plug scheme hostname port usesDefaultPort path query _) = Plug scheme hostname port usesDefaultPort path query Nothing

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
getHost (Plug _ hostname port True _ _ _) = hostname
getHost (Plug _ hostname port False _ _ _) = hostname ++ ":" ++ show port

getPath :: Plug -> String
getPath (Plug _ _ _ _ Nothing _ _) = ""
getPath (Plug _ _ _ _ (Just (segments, True)) _ _) = '/' : joinString "/" (map encodeSegment segments) ++ "/"
getPath (Plug _ _ _ _ (Just (segments, False)) _ _) = '/' : joinString "/" (map encodeSegment segments)

getQuery :: Plug -> String
getQuery (Plug _ _ _ _ _ Nothing _) = ""
getQuery (Plug _ _ _ _ _ (Just kvs) _) = '?' : joinString "&" (map showQueryKeyValue kvs)
    where showQueryKeyValue (k, Nothing) = encodeQuery k
          showQueryKeyValue (k, (Just v)) = encodeQuery k ++ "=" ++ encodeQuery v

getFragment :: Plug -> String
getFragment (Plug _ _ _ _ _ _ Nothing) = ""
getFragment (Plug _ _ _ _ _ _ (Just f)) = '#' : encodeFragment f

instance Show Plug where
    show p = scheme p ++ "://" ++ getHost p ++ getPath p ++ getQuery p ++ getFragment p
