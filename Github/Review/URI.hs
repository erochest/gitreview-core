{-# LANGUAGE RecordWildCards #-}

module Github.Review.URI
    ( toGithubUri
    ) where

import qualified Data.List as L
import           Network.URI

-- Quite inefficient.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace sub repl s@(sh:st)
        | sub `L.isPrefixOf` s = repl ++ replace sub repl (drop (length sub) s)
        | otherwise            = sh : replace sub repl st

updateHost :: String -> String
updateHost h | "api." `L.isPrefixOf` h = drop 4 h
             | otherwise               = h

changeRegName :: (String -> String) -> URIAuth -> URIAuth
changeRegName f auth@URIAuth{..} = auth { uriRegName = f uriRegName }

stripRepos :: String -> String
stripRepos p | "/repos/" `L.isPrefixOf` p = drop 6 p
             | otherwise                  = p

toGithubUri :: URI -> URI
toGithubUri uri@URI{..} =
        uri { uriAuthority = changeRegName updateHost `fmap` uriAuthority
            , uriPath      = replace "commits" "commit" $ stripRepos uriPath
            }

