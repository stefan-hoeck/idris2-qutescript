module Qutescript.Request

import Control.RIO
import Control.RIO.App
import Control.RIO.File
import Data.FilePath.File
import Data.Maybe
import Derive.Prelude
import System

%default total
%language ElabReflection

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

public export
data QuteError : Type where
  UnsetEnvironment : (var : String) -> QuteError
  InvalidMode      : (var,val : String) -> QuteError
  InvalidFile      : (var,val : String) -> QuteError
  InvalidPath      : (var,val : String) -> QuteError
  InvalidNat       : (var,val : String) -> QuteError

public export
data QuteMode = Hints | Command

%runElab derive "QuteMode" [Show,Eq,Ord]

public export
record CommandData where
  constructor C
  url          : String
  title        : String
  tabIndex     : Nat
  count        : Maybe Nat
  selectedText : Maybe String

public export
record HintData where
  constructor H
  url          : String
  currentUrl   : String
  selectedText : Maybe String
  selectedHtml : Maybe String

public export
0 ModeData : QuteMode -> Type
ModeData Hints   = HintData
ModeData Command = CommandData

public export
record Request where
  [noHints]
  constructor R
  mode            : QuteMode
  userAgent       : Maybe String
  fifo            : File Abs
  html            : File Abs
  text            : File Abs
  configDir       : Path Abs
  dataDir         : Path Abs
  downloadDir     : Path Abs
  commandlineText : Maybe String
  version         : String
  otherData       : ModeData mode

--------------------------------------------------------------------------------
--          Processing the Environment
--------------------------------------------------------------------------------

mode : (var,val : String) -> Either QuteError QuteMode
mode _ "hints"   = Right Hints
mode _ "command" = Right Command
mode v s         = Left (InvalidMode v s)

nat : (var,val : String) -> Either QuteError Nat
nat _ "0" = Right 0
nat v s   = case cast {to = Nat} s of
  0 => Left (InvalidNat v s)
  n => Right n

file : (var,val : String) -> Either QuteError (File Abs)
file v s = maybe (Left $ InvalidFile v s) Right $ parse s

path : (var,val : String) -> Either QuteError (Path Abs)
path v s = maybe (Left $ InvalidPath v s) Right $ parse s

string : (var,val : String) -> Either QuteError String
string _ v = Right v

parameters {0 ts : List Type}
           {auto has : Has QuteError ts}

  readEnv :
       ((var,val : String) -> Either QuteError a)
    -> String
    -> App ts a
  readEnv f var = do
    Just str <- getEnv var | Nothing => throw (UnsetEnvironment var)
    injectEither (f var str)

  readOptEnv :
       ((var,val : String) -> Either QuteError a)
    -> String
    -> App ts (Maybe a)
  readOptEnv f var = do
    Just str <- getEnv var | Nothing => pure Nothing
    injectEither (Just <$> f var str)

  commandData : App ts CommandData
  commandData =
    [| C
         (readEnv string "QUTE_URL")
         (readEnv string "QUTE_TITLE")
         (readEnv nat "QUTE_TAB_INDEX")
         (readOptEnv nat "QUTE_COUNT")
         (readOptEnv string "QUTE_SELECTED_TEXT")
    |]

  hintData : App ts HintData
  hintData =
    [| H
         (readEnv string "QUTE_URL")
         (readEnv string "QUTE_CURRENT_URL")
         (readOptEnv string "QUTE_SELECTED_TEXT")
         (readOptEnv string "QUTE_SELECTED_HTML")
    |]

  otherData : (m : QuteMode) -> App ts (ModeData m)
  otherData Hints   = hintData
  otherData Command = commandData

  export
  request : App ts Request
  request = do
    m   <- readEnv mode "QUTE_MODE"
    ua  <- readOptEnv string "QUTE_USER_AGENT"
    fi  <- readEnv file "QUTE_FIFO"
    ht  <- readEnv file "QUTE_HTML"
    te  <- readEnv file "QUTE_TEXT"
    cd  <- readEnv path "QUTE_CONFIG_DIR"
    dad <- readEnv path "QUTE_DATA_DIR"
    dod <- readEnv path "QUTE_DOWNLOAD_DIR"
    cot <- readOptEnv string "QUTE_COMMANDLINE_TEXT"
    v   <- readEnv string "QUTE_VERSION"
    o   <- otherData m
    pure $ R m ua fi ht te cd dad dod cot v o

--------------------------------------------------------------------------------
--          Key-Value pairs
--------------------------------------------------------------------------------

hintPairs : HintData -> List (String,String)
hintPairs (H u cu st sh) =
  [ ("QUTE_URL", u)
  , ("QUTE_CURRENT_URL", cu)
  , ("QUTE_SELECTED_TEXT", fromMaybe "" st)
  , ("QUTE_SELECTED_HTML", fromMaybe "" sh)
  ]

commandPairs : CommandData -> List (String,String)
commandPairs (C u t ti c st) =
  [ ("QUTE_URL", u)
  , ("QUTE_TITLE", t)
  , ("QUTE_TAB_INDEX", show ti)
  , ("QUTE_COUNT", show c)
  , ("QUTE_SELECTED_TEXT", fromMaybe "" st)
  ]

modePairs : (m : QuteMode) -> ModeData m -> List (String,String)
modePairs Hints   = hintPairs
modePairs Command = commandPairs

export
pairs : Request -> List (String,String)
pairs (R m ua fi ht te cd dad dod cot v o) =
  [ ("QUTE_MODE", show m)
  , ("QUTE_USER_AGENT", fromMaybe "" ua)
  , ("QUTE_FIFO", interpolate fi)
  , ("QUTE_HTML", interpolate ht)
  , ("QUTE_TEXT", interpolate te)
  , ("QUTE_CONFIG_DIR", interpolate cd)
  , ("QUTE_DATA_DIR", interpolate dad)
  , ("QUTE_DOWNLOAD_DIR", interpolate dod)
  , ("QUTE_COMMANDLINE_TEXT", fromMaybe "" cot)
  , ("QUTE_VERSION", v)
  ] ++ modePairs m o

--------------------------------------------------------------------------------
--          Running Qutebrowser Scripts
--------------------------------------------------------------------------------

export
printErr : QuteError -> String
printErr (UnsetEnvironment s) = "Environment variable not set: \{s}"
printErr (InvalidMode v s)    = "\{v}: Invalid mode: \{s}"
printErr (InvalidFile v s)    = "\{v}: Invalid file path: \{s}"
printErr (InvalidPath v s)    = "\{v}: Invalid directory: \{s}"
printErr (InvalidNat v s)     = "\{v}: Not a natural number: \{s}"

export %inline
quteRun : All (\x => x -> String) ts -> App ts () -> IO ()
quteRun = runApp . mapProperty (die .)
