{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Monad ((>=>))
import           Data.Aeson (Value, json)
import qualified Data.Attoparsec.ByteString.Streaming as A
import qualified Data.ByteString.Streaming as Q
import           Data.Kanji
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Lens.Micro ((^?))
import           Lens.Micro.Aeson (key, _String)
import qualified Shelly as Sh
import           Streaming
import qualified Streaming.Prelude as S

---

type RIO = ResourceT IO

-- | The directory structure is known ahead of time.
allFiles :: FilePath -> Sh.Sh [FilePath]
allFiles fp = do
  dirs <- Sh.ls . Sh.fromText $ T.pack fp
  fs   <- join <$> traverse Sh.ls dirs
  traverse (fmap (T.unpack . Sh.toTextIgnore) . Sh.absPath) fs

-- | Given a data directory, recursively discover each data file and yield
-- it in a Stream.
files :: FilePath -> Stream (Of FilePath) RIO ()
files = liftIO . Sh.shelly . allFiles >=> S.each

-- | Given an absolute path to a file containing one JSON record per line,
-- stream each line.
jsons :: FilePath -> Stream (Of Value) RIO ()
jsons = void . A.parsed json . Q.readFile

texts :: FilePath -> Stream (Of T.Text) RIO ()
texts fp = S.mapMaybe (^? key "text" . _String) $ S.for (files fp) jsons

elementary :: Stream (Of T.Text) RIO () -> Stream (Of Float) RIO ()
elementary = S.map (elementaryDen . levelDist . mapMaybe kanji . T.unpack)

main :: IO ()
main = do
  putStrLn "Starting..."
  runResourceT (S.sum_ . elementary $ texts "data/out/") >>= print . (/ 1097409)
  putStrLn "Complete."
