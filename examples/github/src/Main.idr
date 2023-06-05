module Main

import Qutescript
import System

%default total

prog : FS => App [QuteError,FileErr] ()
prog = do
  req <- request
  cmds [openIn Tab "https://www.github.com/login"]
  sleep 2
  cmds
    [ InsertText "hans-dampf"
    , fakeKey Tab
    , InsertText "password"
    , fakeKey Return
    ]


main : IO ()
main = quteRun [printErr,printErr] (prog @{local})
