module Qutescript.Command

import Data.String

%default total

public export
data Key : Type where
  Chr    : Char -> Key
  Tab    : Key
  Esc    : Key
  Return : Key

export
Interpolation Key where
  interpolate (Chr c) = singleton c
  interpolate Tab     = "<Tab>"
  interpolate Esc     = "<Esc>"
  interpolate Return  = "<Return>"

public export
FromChar Key where fromChar = Chr

--------------------------------------------------------------------------------
--          Flags and Options
--------------------------------------------------------------------------------

namespace Target

  public export
  data Target : Type where
    Normal        : Target
    Tab           : Target
    BackgroundTab : Target
    Window        : Target

  export
  Interpolation Target where
    interpolate Normal        = ""
    interpolate Tab           = "--tab"
    interpolate BackgroundTab = "--bg"
    interpolate Window        = "--window"

  export
  tgt : Target -> String
  tgt Normal        = "normal"
  tgt Tab           = "tab"
  tgt BackgroundTab = "tab-bg"
  tgt Window        = "window"

public export
data Privacy = Private | NotPrivate

export
Interpolation Privacy where
  interpolate Private    = "--private"
  interpolate NotPrivate = ""

public export
data Security = Secure | NotSecure

export
Interpolation Security where
  interpolate Secure    = "--secure"
  interpolate NotSecure = ""

public export
data Rel = Related | NotRelated

export
Interpolation Rel where
  interpolate Related    = "--related"
  interpolate NotRelated = ""

public export
data Globality = Global | NotGlobal

export
Interpolation Globality where
  interpolate Global    = "--global"
  interpolate NotGlobal = ""


--------------------------------------------------------------------------------
--          Command
--------------------------------------------------------------------------------

public export
data Command : Type where
  Back         : Target -> Command
  ClickElement : Target -> (filter,value : String) -> Command
  Close        : Command
  FakeKeys     : Globality -> List Key -> Command
  Forward      : Target -> Command
  Help         : Target -> (topic : String) -> Command
  InsertText   : String -> Command
  Open         : Target -> Rel -> Privacy -> Security -> String -> Command

public export %inline
open' : String -> Command
open' = Open Normal NotRelated NotPrivate NotSecure

public export %inline
openIn : Target -> String -> Command
openIn l = Open l NotRelated NotPrivate NotSecure

public export %inline
fakeKeys : List Key -> Command
fakeKeys = FakeKeys NotGlobal

public export %inline
fakeKey : Key -> Command
fakeKey = fakeKeys . pure

export
Interpolation Command where
  interpolate (Back x)             = "back \{x}"
  interpolate (ClickElement x f v) = "click-element -t \{tgt x} \{f} \{v}"
  interpolate Close                = "close"
  interpolate (FakeKeys g ks)      = "fake-key \{g} \{concat $ map interpolate ks}"
  interpolate (Forward x)          = "forward \{x}"
  interpolate (Help x t)           = "help \{x} \{t}"
  interpolate (InsertText s)       = "insert-text \{s}"
  interpolate (Open l r p s u)     = "open \{l} \{r} \{p} \{s} \{u}"
