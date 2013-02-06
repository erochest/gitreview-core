module Github.Review.URISpec where

import           Control.Applicative
import qualified Data.List as L
import           Data.Maybe
import           Github.Review
import           Network.URI
import           Test.Hspec
-- import           Test.QuickCheck

testUrl :: String
testUrl = "https://api.github.com/repos/scholarslab/SolrSearch/commits/df5618356282acedec1d603b115e29b1a5065092"

expectedUrl :: String
expectedUrl = "https://github.com/scholarslab/SolrSearch/commit/df5618356282acedec1d603b115e29b1a5065092"

url :: Maybe URI
url = toGithubUri <$> parseURI testUrl

shouldNotContain :: Maybe URI -> (URI -> String) -> String -> Expectation
shouldNotContain uri getter s =
        getter (fromJust uri) `shouldSatisfy` (not . L.isInfixOf s)

shouldContain :: Maybe URI -> (URI -> String) -> String -> Expectation
shouldContain uri getter s =
        getter (fromJust uri) `shouldSatisfy` L.isInfixOf s

spec :: Spec
spec =
    describe "getGithubUri" $ do
        it "should properly parse the URL." $
            url `shouldSatisfy` isJust
        it "should return the expected output." $
            show (fromJust url) `shouldBe` expectedUrl
        context "the host" $
            it "should not contain the string 'api'." $
                (url `shouldNotContain` (uriRegName . fromJust . uriAuthority)) "api"
        context "the path" $ do
            it "should not contain the string 'repos'." $
                (url `shouldNotContain` uriPath) "repos"
            it "should not contain the string 'commits'." $
                (url `shouldNotContain` uriPath) "commits"
            it "should contain the string 'commit'." $
                (url `shouldContain` uriPath) "commit"
                

