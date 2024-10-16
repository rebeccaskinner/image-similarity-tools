module Main where
import PerceptualHash
import Data.PerceptualHash
import Data.Foldable
import System.FilePath
import System.Environment (getArgs)

displayPerceptualHash :: FilePath -> FilePath -> IO ()
displayPerceptualHash outDir fname = do
  -- let basename = takeFileName fname
  --     outname = outDir </> basename
  -- roundtripImageFile fname outname
  phash <- perceptualHashFile fname
  putStrLn $ fname <> ":" <> showHex phash

main :: IO ()
main = do
  (outDir:fnames) <- getArgs
  traverse_ (displayPerceptualHash outDir) fnames
