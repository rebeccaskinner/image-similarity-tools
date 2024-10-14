module Main where
import PerceptualHash
import Data.PerceptualHash
import System.Environment (getArgs)

liftEither :: IO (Either String a) -> IO a
liftEither val = do
  Right val' <- val
  pure val'

main :: IO ()
main = do
  [inFname, outFname] <- getArgs
  roundtripImageFile inFname outFname
  phash <- liftEither $ perceptualHashFile inFname
  putStrLn $ inFname <> ":" <> showHex phash
