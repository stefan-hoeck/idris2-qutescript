module Qutescript

import public Control.RIO
import public Control.RIO.File
import public Qutescript.Command
import public Qutescript.Request

parameters {auto h : Has FileErr ts}
           {auto r : Request}

  export %inline
  cmd : Command -> App ts ()
  cmd c = write @{local} r.fifo "\{c}\n"

  export %inline
  cmds : List Command -> App ts ()
  cmds = traverse_ cmd
