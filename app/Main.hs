module Main where

import Lib
import WServer
import TestJson ( printReq, printRes, getClientId )

main :: IO ()
main = do 
    print printReq
    print printRes
    print  getClientId
-- main = WServer.runServer
