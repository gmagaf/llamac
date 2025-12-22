module Common.FileUtils (readFileB, safeReadFile, writeToFile) where

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import Control.Exception (IOException, handle)


-- Utils for files
readFileB :: FilePath -> IO String
readFileB fileName = do
  bts <- B.readFile fileName
  return (T.unpack . T.decodeUtf8 $ bts)

safeReadFile :: FilePath -> IO (Either String String)
safeReadFile fileName = handle handleEx (Right <$> readFileB fileName)
  where handleEx :: IOException -> IO (Either String String)
        handleEx e = return (Left (show e))

writeToFile :: FilePath -> String -> IO ()
writeToFile filePath content = do
  let bString = T.encodeUtf8 . T.pack $ content
  B.writeFile filePath bString