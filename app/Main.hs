{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import qualified Data.ByteString as B
import Lib
import Network.Simple.TCP
import Options.Applicative (execParser)
import System.IO.Error

flood :: String -> String -> IO ()
flood host port =
  (connect host port $ \(connectionSocket, _) -> recv connectionSocket 100) >>=
  (\x ->
     case x of
       Just y -> B.putStrLn y
       _ -> B.putStrLn "Nothing received")

floodWithException :: String -> String -> IO Bool
floodWithException host port =
  catchIOError
    (flood host port >> return True)
    (\x -> (Prelude.putStrLn . ("Exception > " ++) . show $ x) >> return False)

parallelExec :: String -> String -> Int -> IO [Bool]
parallelExec host port times = mapConcurrently (\_ -> floodWithException host port) [0 .. times]

main :: IO ()
main = execParser programCli >>= \a -> parallelExec (host a) (port a) (times a) >>= putStrLn . stats
