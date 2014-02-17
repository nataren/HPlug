{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Uri (
    usesDefaultPort,
    withScheme,
    withCredentials,
    withoutCredentials,
    withHostname,
    withPort,
    withTrailingSlash,
    withoutTrailingSlash,
    at,
    with,
    with',
    without,
    withParams,
    withoutQuery,
    getParam,
    getParams,
    withFragment,
    withoutFragment,
    getScheme,
    getAuthority,
    getHost,
    getPath,
    getQuery,
    getFragment,
    show,
    (&)
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toUpper)
import Data.List (intercalate)
import Network.URI (escapeURIString)

equalsIgnoreCase :: String -> String -> Bool
equalsIgnoreCase a b = map toUpper a == map toUpper b

-- let p = Uri { scheme = "http", username = Nothing, password = Nothing, hostname = "example.com", port = 80, path = Nothing, query = Nothing, fragment = Nothing }
-- p `withScheme` "https" `withCredentials` ("john", "pwd") `withPort` 443 `at` "x" `at` "y" `with` ("a", Just "b") `withFragment` "foo"

data Uri = Uri {
    scheme          :: String,
    username        :: Maybe String,
    password        :: Maybe String,
    hostname        :: String,
    port            :: Int,
    path            :: Maybe ([String], Bool),
    query           :: Maybe [(String, Maybe String)],
    fragment        :: Maybe String
} deriving (Eq)

-- TODO (steveb): need to fix this; this function is about determinig if the original text had an explicit port or not
isDefaultPort :: String -> Int -> Bool
isDefaultPort scheme' 80 = scheme' `equalsIgnoreCase` "http"
isDefaultPort scheme' 443 = scheme' `equalsIgnoreCase` "https"
isDefaultPort scheme' 21 = scheme' `equalsIgnoreCase` "ftp"
isDefaultPort _ _ = False

usesDefaultPort :: Uri -> Bool
usesDefaultPort uri = isDefaultPort (scheme uri) (port uri)

withScheme :: Uri -> String -> Uri
withScheme uri scheme' = uri { scheme = scheme' }

withCredentials :: Uri -> (String, String) -> Uri
withCredentials uri (username', password') = uri { username = Just username', password = Just password' } 

withoutCredentials :: Uri -> Uri
withoutCredentials uri@Uri { username = Nothing, password = Nothing } = uri
withoutCredentials uri = uri { username = Nothing, password = Nothing }

withHostname :: Uri -> String -> Uri
withHostname uri hostname' = uri { hostname = hostname' }

withPort :: Uri -> Int -> Uri
withPort uri port' = uri { port = port' }

withTrailingSlash :: Uri -> Uri
withTrailingSlash uri@Uri { path = Nothing } = uri { path = Just ([], True) }
withTrailingSlash uri@Uri { path = Just (_, True) } = uri
withTrailingSlash uri@Uri { path = Just (segments', False) } = uri { path = Just (segments', True) }

withoutTrailingSlash :: Uri -> Uri
withoutTrailingSlash uri@Uri { path = Nothing } = uri
withoutTrailingSlash uri@Uri { path = Just (segments', True) } = uri { path = Just (segments', False) }
withoutTrailingSlash uri@Uri { path = Just (_, False ) } = uri

at :: Uri -> String -> Uri
at uri@Uri { path = Nothing } segment = uri { path = Just ([ segment ], False)}
at uri@Uri { path = Just (segments', trailingSlash') } segment = uri { path = Just (segments' ++ [ segment ], trailingSlash')}

with :: Uri -> (String, Maybe String) -> Uri
with uri@Uri { query = Nothing } kv = uri { query = Just [ kv ]}
with uri@Uri { query = Just kvs } kv = uri { query = Just (kvs ++ [ kv ])}

with' :: Uri -> (String, String) -> Uri
with' uri@Uri { query = Nothing } (k, v) = uri { query = Just [ (k, Just v) ]}
with' uri@Uri { query = Just kvs } (k, v) = uri { query = Just (kvs ++ [ (k, Just v) ])}

without :: Uri -> String -> Uri
without uri@Uri { query = Nothing } _ = uri
without uri@Uri { query = Just queryParams } key = uri { query = if null queryParams' then Nothing else Just queryParams' }
    where matchesKey (key', _) = not (key `equalsIgnoreCase` key')
          queryParams' = filter matchesKey queryParams

withParams :: Uri -> [(String, Maybe String)] -> Uri
withParams uri@Uri { query = Nothing } kvs = uri { query = Just kvs }
withParams uri@Uri { query = Just kvs' } kvs = uri { query = Just (kvs' ++ kvs) }

-- TODO
-- withQuery :: Uri -> String -> Uri

withoutQuery :: Uri -> Uri
withoutQuery uri = uri { query = Nothing }

getParam :: Uri -> String -> Maybe (Maybe String)
getParam Uri { query = Nothing } _ = Nothing
getParam uri@Uri { query = Just _ } key = if null matchedParams then Nothing else Just $ head matchedParams
    where matchedParams = uri `getParams` key

getParams :: Uri -> String -> [ Maybe String ]
getParams Uri { query = Nothing } _ = [ ]
getParams Uri { query = Just queryParams } key = map selectValue $ filter matchesKey queryParams
    where matchesKey (key', _) = key `equalsIgnoreCase` key'
          selectValue (_, value) = value

withFragment :: Uri -> String -> Uri
withFragment uri fragment' = uri { fragment = Just fragment' }

withoutFragment :: Uri -> Uri
withoutFragment uri@Uri { fragment = Nothing } = uri
withoutFragment uri = uri { fragment = Nothing }

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

getScheme :: Uri -> String
getScheme uri = scheme uri ++ "://"

getUserInfo :: Uri -> String
getUserInfo Uri { username = Nothing, password = Nothing } = ""
getUserInfo Uri { username = Nothing, password = Just password' } = ':' : encodeUserInfo password' ++ "@"
getUserInfo Uri { username = Just username', password = Nothing } = encodeUserInfo username' ++ "@"
getUserInfo Uri { username = Just username', password = Just password' } = encodeUserInfo username' ++ ":" ++ encodeUserInfo password' ++ "@"

getHost :: Uri -> String
getHost uri = hostname uri ++ (if usesDefaultPort uri then "" else ':' : show (port uri) )

getAuthority :: Uri -> String
getAuthority uri = getUserInfo uri ++ getHost uri

getPath :: Uri -> String
getPath Uri { path = Nothing } = ""
getPath Uri { path = Just (segments', True) } = '/' : intercalate "/" (map encodeSegment segments') ++ "/"
getPath Uri { path = Just (segments', False) } = '/' : intercalate "/" (map encodeSegment segments')

getQuery :: Uri -> String
getQuery Uri { query = Nothing } = ""
getQuery Uri { query = Just kvs } = '?' : intercalate "&" (map showQueryKeyValue kvs)
    where showQueryKeyValue (k, Nothing) = encodeQuery k
          showQueryKeyValue (k, Just v) = encodeQuery k ++ "=" ++ encodeQuery v

getFragment :: Uri -> String
getFragment Uri { fragment = Nothing } = ""
getFragment Uri { fragment = Just fragment' } = '#' : encodeFragment fragment'

instance Show Uri where
    show uri = getScheme uri ++ getAuthority uri ++ getPath uri ++ getQuery uri ++ getFragment uri

class UriAppend a where
    (&) :: Uri -> a -> Uri

instance UriAppend String where
    (&) uri segment = uri `at` segment

instance UriAppend (String, Maybe String) where
    (&) uri kv = uri `with` kv

instance UriAppend (String, String) where
    (&) uri (k, v) = uri `with` (k, Just v)
