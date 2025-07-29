{-#LaNgUaGe TupleSections #-}
module DataFile 
( readData
) where 

import qualified Text.XML.Light as XML
import qualified Text.XML.Light.Cursor as Cursor
import qualified Data.ByteString as BS
import Text.Read (readMaybe)
import Control.Monad ((>=>))
import Data.Foldable (toList)

named :: String -> XML.Element -> [XML.Element]
named s = XML.filterChildrenName ((== s) . XML.qName)

findEstablishmentDetails :: XML.Element -> [XML.Element]
findEstablishmentDetails = 
    named "EstablishmentCollection" >=> named "EstablishmentDetail"

namedOne :: String -> XML.Element -> Maybe XML.Element
namedOne s = XML.filterChildName ((== s) . XML.qName)

readPath :: Read a => [String] -> XML.Element -> Maybe a
readPath (x:xs) e = readPath xs =<< namedOne x e
readPath [] e = readMaybe . foldMap XML.cdData . XML.onlyText . XML.elContent $ e

getLon :: XML.Element -> Maybe Double
getLon = readPath ["Geocode", "Longitude"]

getLat :: XML.Element -> Maybe Double 
getLat = readPath ["Geocode", "Latitude"]

getScore :: XML.Element -> Maybe Int 
getScore = readPath ["RatingValue"]

readRow :: XML.Element -> Maybe (Double, Double, Int)
readRow e = (,,) <$> getLon e <*> getLat e <*> getScore e

readData :: String -> IO [(Double, Double, Int)]
readData filename = do
    bs <- BS.readFile filename
    let Just doc = XML.parseXMLDoc bs 
    return ((toList . readRow) =<< findEstablishmentDetails doc)