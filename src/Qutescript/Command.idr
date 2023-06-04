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

namespace Location

  public export
  data Location : Type where
    Here          : Location
    Tab           : Location
    BackgroundTab : Location
    Window        : Location

  export
  Interpolation Location where
    interpolate Here          = ""
    interpolate Tab           = "--tab"
    interpolate BackgroundTab = "--bg"
    interpolate Window        = "--window"

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
  Back       : Location -> Command
  Forward    : Location -> Command
  Help       : Location -> (topic : String) -> Command
  Open       : Location -> Rel -> Privacy -> Security -> String -> Command
  FakeKeys   : Globality -> List Key -> Command
  InsertText : String -> Command

public export %inline
open' : String -> Command
open' = Open Here NotRelated NotPrivate NotSecure

public export %inline
openIn : Location -> String -> Command
openIn l = Open l NotRelated NotPrivate NotSecure

public export %inline
fakeKeys : List Key -> Command
fakeKeys = FakeKeys NotGlobal

public export %inline
fakeKey : Key -> Command
fakeKey = fakeKeys . pure

export
Interpolation Command where
  interpolate (Back x)         = "back \{x}"
  interpolate (Forward x)      = "forward \{x}"
  interpolate (Help x t)       = "help \{x} \{t}"
  interpolate (Open l r p s u) = "open \{l} \{r} \{p} \{s} \{u}"
  interpolate (FakeKeys g ks)  = "fake-key \{g} \{concat $ map interpolate ks}"
  interpolate (InsertText s)   = "insert-text \{s}"
