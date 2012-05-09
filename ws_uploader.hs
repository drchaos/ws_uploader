import Prelude
import Network.HTTP.Enumerator
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Export
import Text.XML.Light
import Text.XML.Light.Proc
import Text.XML.Light.Types
import Text.XML.Light.Lexer
import qualified Data.ByteString.Lazy as L
import qualified System.IO.UTF8 as U
import Data.ByteString.Lazy.UTF8
import Codec.Binary.UTF8.String
import Control.Monad
import Control.Arrow
import Data.Maybe
import Data.List

--import Control.Monad.Trans

getAttrVal name = findAttrByName name >>> fromJust >>> attrVal 
    where findAttrByName name el = find (\a -> (qName.attrKey) a == name ) attrs
            where attrs =  elAttribs el
getCollectionEl doc =  head $ filterElements (\el -> (qName.elName) el == "collection"  &&  getAttrVal "id" el == "album-list" ) doc
urlFromEl = getAttrVal "href"
--main::IO()
main = do
    s <- simpleHttp "http://api-fotki.yandex.ru/api/users/mrschaos/"
    feed <- simpleHttp $ urlFromEl $ getCollectionEl $ fromJust $ parseXMLDoc s
    --L.putStrLn feed
    let items = getFeedItems $ fromJust $ readAtom $ fromJust $ parseXMLDoc feed
    albumFeed <- parseUrl $ (decodeString.fromJust.getItemLink)  $ fromJust$ (find (\item -> (fromJust.getItemTitle) item == encodeString "Кошки  и собаки") ) items
    print albumFeed
