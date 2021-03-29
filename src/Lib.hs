module Lib
    ( someFunc
    ) where

import Graph

foo :: Graph Int String
foo = read "\"foo\"\n\
  \  8  -> \"bar\"\n\
  \  10 -> \"qua\"\n\
  \    11 -> \"zip\"\n\
  \  12 -> \"pot\"\n\
  \"

someFunc :: IO ()
someFunc = print foo
