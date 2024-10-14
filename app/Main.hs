module Main where
import PerceptualHash
import Data.PerceptualHash
import System.Environment (getArgs)

liftEither :: Either String a -> IO a
liftEither (Left err) = ioError $ userError err
liftEither (Right val) = pure val

main :: IO ()
main = do
  [inFname, outFname] <- getArgs
  roundTripImageFile inFname outFname
  phash <- liftEither $ perceptualHashFile inFname
  putStrLn $ inFname <> ":" <> showHex phash
