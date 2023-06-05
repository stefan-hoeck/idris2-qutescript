module Main

import Qutescript

%default total

htmlBody : Request -> String
htmlBody req =
  let content := map (\(n,v) => "<li>\{n}: \{v}</li>") (pairs req)
   in """
      <html>
      <head><title>Qutescript</title></head>
      <body><ul>\{fastUnlines content}</ul></body>
      """

prog : FS => App [QuteError,FileErr] ()
prog = do
  req <- request
  tmp <- map (/> "tmp_debug.html") curDir
  write tmp (htmlBody req)
  cmd $ openIn Tab "file://\{tmp}"

main : IO ()
main = quteRun [printErr,printErr] (prog @{local})
