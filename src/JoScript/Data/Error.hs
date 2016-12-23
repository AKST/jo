module JoScript.Data.Error where

import JoScript.Data.Position

data Error = Error Kind Location

data Location = Known Position

data Kind = IndentError IndentErrorT

data IndentErrorT
  = ShallowDedent

known k p = Error k (Known p)
