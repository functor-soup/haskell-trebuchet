module Lib
  ( Params(..)
  , sample
  , programCli
  , stats
  ) where

import qualified Data.List as D
import Data.Semigroup ((<>))
import Options.Applicative

data Params = Params
  { host :: String
  , port :: String
  , times :: Int
  }

sample :: Parser Params
sample =
  Params <$> strOption (long "host" <> help "url to overwhelm with requests") <*>
  strOption (long "port" <> short 'q' <> help "port number") <*>
  option auto (long "number" <> short 'n' <> help "number of requests to send")

programCli :: ParserInfo Params
programCli =
  info
    (sample <**> helper)
    (fullDesc <> progDesc "server stress tester" <>
     header "haskell-trebuchet - a dead simple stress tester of servers")

stats :: [Bool] -> String
stats x =
  let (t, f) = D.partition id x
      [lt, lf] = map (fromIntegral . length) [t, f]
      len = lt + lf
      tl = "Total number of requests \t\t = " ++ (show $ len)
      l1 = "Number of failed requests \t\t = " ++ (show $ lf)
      l1p = "% of failed requests \t\t\t = " ++ (show $ lf / len)
      l2 = "Number of non-failed requests \t\t = " ++ (show $ lt)
      l2p = "% of non-failed requests \t\t = " ++ (show $ lt / len)
  in D.intercalate "\n" [tl, l1, l1p, l2, l2p]
