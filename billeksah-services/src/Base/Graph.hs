-----------------------------------------------------------------------------
--
-- Module      :  Base.Graph
-- Copyright   :  Juergen "jutaro" Nicklisch-Franken
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Base.Graph where


import Data.Map (Map)
import Data.Graph
       (edges, graphFromEdges, Vertex, Graph, topSort, transposeG)
import Data.List ((\\), nub)
import qualified Data.Map as Map (insert, lookup, empty, toList)



type MyGraph a = Map a [a]

reverseGraph :: Ord alpha => MyGraph alpha -> MyGraph alpha
reverseGraph = withIndexGraph transposeG

topSortGraph :: Ord alpha => MyGraph alpha -> [alpha]
topSortGraph myGraph =  map ((\ (_,x,_)-> x) . lookup) $ topSort graph
  where
    (graph,lookup,_) = fromMyGraph myGraph

withIndexGraph :: Ord alpha => (Graph -> Graph) -> MyGraph alpha -> MyGraph alpha
withIndexGraph idxOp myGraph = toMyGraph (idxOp graph) lookup
  where
    (graph,lookup,_) = fromMyGraph myGraph

fromMyGraph :: Ord alpha => MyGraph alpha -> (Graph, Vertex -> ((), alpha , [alpha]), alpha -> Maybe Vertex)
fromMyGraph myGraph =
    graphFromEdges
        $ map (\(e,l)-> ((),e,l))
            $ graphList ++ map (\e-> (e,[])) missingEdges
  where
    mentionedEdges = nub $ concatMap snd graphList
    graphList = Map.toList myGraph
    missingEdges = mentionedEdges \\ map fst graphList

toMyGraph ::  Ord alpha =>  Graph -> (Vertex -> ((), alpha, [alpha])) -> MyGraph alpha
toMyGraph graph lookup = foldr constr Map.empty myEdges
  where
    constr (from,to) map = case Map.lookup from map of
                                Nothing -> Map.insert from [to] map
                                Just l -> Map.insert from (to : l) map
    myEdges              = map (\(a,b) -> (lookItUp a, lookItUp b)) $ edges graph
    lookItUp             =  (\(_,e,_)-> e) . lookup

