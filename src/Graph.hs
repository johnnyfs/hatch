module Graph (
    Graph (..)
  ) where

import Control.Exception
import Data.Map

newtype GraphParseException = GraphParseException String deriving Show
instance Exception GraphParseException

data Graph e n = Node n (Map e (Graph e n)) deriving Show

instance (Ord e, Read e, Read n) => Read (Graph e n) where
  readsPrec _ s = readGraph s

{-
  Example read:

  "foo"              -- root node
    8 -> "bar"       -- edge from root to "bar" via 8
    10 -> "zip"      -- edge from root to "zip" via 10
      11 -> "pot"    -- edge from "zip" to "pot" via 11
    9 -> "qua"       -- edge from root to "qua" via 9

  Yields a graph of node type String, edge type Int
-}
readGraph s = readGraph' 0 s

readGraph' indent s = do
  (node, s') <- reads s
  (_, s'') <- expectNewline s'
  (edges, s''') <- readEdges indent s''
  return (Node node (fromList edges), s''')

readEdges indent s = do
  (indent', s') <- countIndent s
  if indent' <= indent
    then return ([], s)
    else do
      (edge, s'') <- readEdge indent' s'
      (edges, s''') <- readEdges indent s''
      return (edge:edges, s''')

countIndent (' ':s) = do
  (n, s') <- countIndent s
  return (n + 1, s')
countIndent s = return (0, s)

readEdge indent s = do
  (edge, s') <- reads s
  (_, s'') <- expectArrow s'
  (graph, s''') <- readGraph' indent s''
  return ((edge, graph), s''')

expectArrow s = do
  (_, s') <- eatWhitespace s
  case s' of
    ('-':'>':s'') -> eatWhitespace s''
    _ -> throw $ GraphParseException $ "expected edge arrow(->) at " ++ s'

eatWhitespace (' ':s) = eatWhitespace s
eatWhitespace s = return ((),s)

expectNewline "" = return ((), "")
expectNewline ('\n':s) = return ((), s)
expectNewline (' ':s) = expectNewline s
expectNewline s = throw $ GraphParseException $ "expected newline or end-of-string at " ++ s
