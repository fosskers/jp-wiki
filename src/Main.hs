{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Monad ((>=>))
import           Control.Monad.Trans.Resource
import           Data.Aeson (Value, json)
import qualified Data.Attoparsec.ByteString.Streaming as A
import qualified Data.ByteString.Streaming as Q
import           Data.Kanji hiding (densities)
import           Data.Maybe (mapMaybe)
import           Data.Semigroup
import qualified Data.Text as T
import           Lens.Micro
import           Lens.Micro.Aeson (key, _String)
import qualified Shelly as Sh
import           Streaming hiding (Sum, (<>))
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

data Densities = Densities !Float !Float !Float deriving (Show)

instance Semigroup Densities where
  Densities a b c <> Densities a' b' c' = Densities (a + a') (b + b') (c + c')

instance Monoid Densities where
  mempty  = Densities 0 0 0
  mappend = (<>)

densities :: T.Text -> Densities
densities t = Densities e (m - e) (h - m)
  where ld = levelDist . mapMaybe kanji $ T.unpack t
        e  = elementaryDen ld
        m  = middleDen ld
        h  = highDen ld

main :: IO ()
main = do
  putStrLn "Starting..."
  Densities e m h <- runResourceT . S.mconcat_ . S.map densities $ texts "data/out/"
  putStrLn $ "Elementary: "    <> show (100 * e / 1097409)
  putStrLn $ "Middle School: " <> show (100 * m / 1097409)
  putStrLn $ "High School: "   <> show (100 * h / 1097409)
  putStrLn "Complete."
