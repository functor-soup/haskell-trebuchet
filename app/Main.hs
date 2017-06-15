{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Data.ByteString as B
import Data.Semigroup ((<>))
import Network.Simple.TCP
import Options.Applicative
import System.IO.Error

import Lib

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

flood :: String -> String -> IO ()
flood host port =
  (connect host port $ \(connectionSocket, _) -> recv connectionSocket 100) >>=
  (\x ->
     case x of
       Just y -> B.putStrLn y
       _ -> B.putStrLn "Nothing received")

floodWithException :: String -> String -> IO ()
floodWithException host port =
  catchIOError (flood host port) (Prelude.putStrLn . ("Exception > " ++) . show)

parallelExec :: String -> String -> Int -> IO ()
parallelExec host port times = mapConcurrently_ (\_ -> floodWithException host port) [0 .. times]

main :: IO ()
main = execParser programCli >>= \a -> parallelExec (host a) (port a) (times a)
