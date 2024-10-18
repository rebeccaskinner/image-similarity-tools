module Main where
import PerceptualHash
import Data.PerceptualHash
import Data.Foldable
import System.FilePath
import System.Environment (getArgs)
import Data.BKTree qualified as BK
import Data.Map.Strict qualified as Map

loadPerceptualHash :: FilePath -> IO (PerceptualHash, FilePath)
loadPerceptualHash fname = (, fname) <$> perceptualHashFile fname

main :: IO ()
main = do
  (needle:haystack) <- getArgs
  (needleHash, _) <- loadPerceptualHash needle
  haystackKVs <- filter ((/= PerceptualHash 0) . fst) <$> traverse loadPerceptualHash haystack
  let
    haystackMap = Map.fromList haystackKVs
    haystackTree = BK.fromList perceptualHashDistance (fst <$> haystackKVs)
  writeFile "db.dot" $ BK.ppTree haystackTree
  putStrLn "wrote reference image db as dot file..."
  let Just (bestHash, distance) = BK.bestMatch needleHash haystackTree
  putStrLn $ "searched for " <> showHex needleHash <> " best match " <> showHex bestHash <> " d=" <> show distance
  putStrLn $ show needle <> ": best matching filename: " <> show (haystackMap Map.!? bestHash)
