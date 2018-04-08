{-# LANGUAGE LambdaCase #-}
module Main where


import System.IO (stdin, hSetEcho, hReady, stdout)
import GHC.IO.Handle
import Control.Monad (when)
import System.Process
import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent
import Control.Monad


getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

type Commands = TMVar [String]

ghci :: Commands -> IO ()
ghci cmds = createProcess ((proc "ghci" ["-ignore-dot-ghci"])) {std_in=CreatePipe} >>= \case
    (Just pin, Nothing, Nothing, ph) -> processCommandsLoop cmds pin
    _ -> error "Something went wrong"

processCommandsLoop :: Commands -> Handle ->  IO ()
processCommandsLoop cmds input = do
  x <- atomically $ takeTMVar cmds
  forM x (\x' -> do
    print "-- ## Sending command to process:"
    print x'
    hPutStr input (x' ++ "\n")
    hFlush input
    )
  hFlush stdout
  processCommandsLoop cmds input

mainLoop :: TMVar [String] -> IO ()
mainLoop commands = do
  key <- getKey
  when (key /= "\ESC") $ do
    case key of
      -- "s" -> do
      --   putStrLn "-- ## Starting!"
      --   ghci
      "r" -> putStrLn "-- ## Reloading!"
      "t" -> do
        putStrLn "-- ## Testing!"
        atomically $ putTMVar commands ([":t head", ":t tail"])
      "h" -> putStrLn "-- ## Hushing!"
      "d" -> do
        putStrLn "-- ## Debug!"
        (atomically $ readTMVar commands) >>= print
      _        -> return ()
  mainLoop commands

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  cmds <- newEmptyTMVarIO
  ghciThread <- forkIO $ ghci cmds
  print "Started"
  hSetEcho stdin False
  mainLoop cmds
