import Prelude
import Network.HTTP.Enumerator
import Text.XML.Light
import Text.XML.Light.Proc
import Text.XML.Light.Types
import qualified Data.ByteString.Lazy as L
import Control.Monad
import Control.Arrow
import Data.Maybe
import Data.List

--import Control.Monad.Trans

getAttrVal name = findAttrByName name >>> fromJust >>> attrVal 
	where findAttrByName name el = find (\a -> (qName.attrKey) a == name ) attrs
				where attrs =  elAttribs el
main::IO()
-- main =  getTopEl  "http://api-fotki.yandex.ru/api/users/mrschaos/" >>= putstr
main = do
	s <- simpleHttp "http://api-fotki.yandex.ru/api/users/mrschaos/"
	L.putStr s
	let doc = fromJust $ parseXMLDoc s in
		putStr $ show $ urlFromEl $ element doc
		where element doc = head $ filterElements (\el -> (qName.elName) el == "collection"  &&  getAttrVal "id" el == "album-list" ) doc
		      urlFromEl = getAttrVal "href"


