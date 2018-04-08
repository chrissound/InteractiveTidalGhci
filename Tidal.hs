{-# Language OverloadedStrings #-}
module Tidal where

import Sound.Tidal.Context

main :: IO ()
main = do
  print "Starting..."

chris :: IO ()
chris = print "abcxyz"

doAction :: IO ()
doAction = chris

d0Action, d1Action :: (ParamPattern -> IO ()) -> IO ()
d0Action d = d $ sound "bd ~"
d1Action d = d $ sound "~ [hh hh]"
