module Main where

import Lib
import WServer
import TestJson

main :: IO ()
main = do 
    print printReq
    print printRes
-- main = WServer.runServer
