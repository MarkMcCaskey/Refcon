module EdictDB where

import System.IO
import qualified Data.Text as DT
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as C

type Word = (String, Char)


dbLookup :: String -> Maybe Word
dbLookup = undefined
 
returnLine :: IO String -> String
returnLine = undefined

getDict :: IO String
getDict = do
  y <- openFile "edict" ReadMode
  hSetEncoding y latin1
  z <- hGetContents y
  let k = decodeLatin1 $ C.pack z 
  hClose y
  return $ DT.unpack k
