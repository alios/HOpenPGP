
import qualified Data.Text as T

import Data.Maybe (fromJust)
import Network.URI (parseURI)
import Network.HTTP
import Network.Browser
import qualified Data.ByteString.Lazy as B
import System.IO

baseuri s = 
  let q = urlEncodeVars [("tl", "de"), ("q", s)]
  in fromJust $ parseURI $ "http://translate.google.com/translate_tts?" ++ q 
     
isPunct c = or [ c == c' | c' <- ".?!"]

ba :: String -> BrowserAction (HandleStream B.ByteString) [B.ByteString]
ba s = do
  setAllowRedirects True
  rsps <- sequence $ map req $ t2s s
  return $ map (rspBody.snd) rsps 

req s = request $ Request (baseuri s) GET [] B.empty

t2s :: String -> [String]
t2s s = map T.unpack $ T.split isPunct $ T.pack s
          
main = do
  t <- getContents
  ds <- fmap B.concat $ browse $ ba t
  B.writeFile "/tmp/out.mp3" ds
  