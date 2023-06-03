module Qutescript

import public Control.RIO
import public Control.RIO.File
import public Qutescript.Request

export %inline
cmd : Has FileErr ts => String -> Request -> App ts ()
cmd c r = write @{local} r.fifo c
