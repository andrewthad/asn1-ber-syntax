import Asn.Ber (decode)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16)
import Data.Bytes (Bytes)
import Data.Char (isSpace)
import System.FilePath (takeBaseName, replaceExtension)
import System.IO (hPrint,withFile,IOMode(WriteMode))
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile, findByExtension)
import Text.Pretty.Simple (pHPrint)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Bytes as Bytes
import qualified GHC.Exts as Exts

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  files <- findByExtension [".input"] "golden"
  return $ testGroup "ber" $ flip map files $ \inputName -> do
    let actualName = replaceExtension inputName ".actual"
    let expectedName = replaceExtension inputName ".expected"
    goldenVsFile (takeBaseName inputName) expectedName actualName $ do
      b16 <- ByteString.readFile inputName
      case decodeBase16 (BC8.filter (not . isSpace) b16) of
        Left _ -> fail "file contained non-hexadecimal characters"
        Right b -> case decode (bytestringToBytes b) of
          Left err -> fail ("could not decode input: " ++ err)
          Right s -> withFile actualName WriteMode (\h -> pHPrint h s)

bytestringToBytes :: ByteString -> Bytes
bytestringToBytes = Exts.fromList . ByteString.unpack
