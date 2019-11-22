{-
 - Ogg
 - Generates
 - Gcode
 -}

import System.Environment
data MY_BOOL =
    T
  | F

  deriving (Show)

xor :: MY_BOOL ->  MY_BOOL -> MY_BOOL
xor a b =
      case a of
            T -> case b of
                  F -> T
                  T -> F
            F -> case b of
                  T -> T
                  F -> F
            
main = 
      {-print (xor (F, T))-}
      print (xor F T)
