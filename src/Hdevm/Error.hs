module Hdevm.Error where


data DeError = InvalidOpcode String
             | InvalidJumpDest Int
             deriving Show
