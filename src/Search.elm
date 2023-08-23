module Search exposing (resolutionMethod)

import Disjunct
import DisjunctSet exposing (DisjunctSet)
import ResolutionStep exposing (LogEntry, resolvents, colapses)
import PriorityQueue exposing (PriorityQueue)
import ListHelperFunctions exposing (..)
-- import Heuristic exposing (minPseudoResolutionSteps)

type alias Node =
    { disjuncts : DisjunctSet
    , gScore : Int
    , log : List LogEntry
    }

-- simple heuristic that finds the minimum size of disjunct
hScore : Node -> Int
hScore node =
    DisjunctSet.foldl (\ d1 min ->
        let
            d1Size = Disjunct.size d1
        in 
        if d1Size < min then d1Size else min
    ) (2^53 - 1) node.disjuncts
    -- minPseudoResolutionSteps node.disjuncts

generateChildren : Node -> List Node
generateChildren node = 
    let
        disjuncts = DisjunctSet.toList node.disjuncts
    in
    subsetsOf2 disjuncts
        |> List.concatMap (\ (d1, d2) -> resolvents d1 d2)
        |> List.append (disjuncts |> List.concatMap (\ d -> colapses d))
        |> List.map (\ (d, logEntry) -> {   
                disjuncts = DisjunctSet.insert d node.disjuncts, 
                gScore = node.gScore + 1,
                log = List.append node.log [logEntry]
            }
        )

final : Node -> Bool
final node = 
    DisjunctSet.any Disjunct.isEmpty node.disjuncts

aStarLoop : PriorityQueue Node -> Maybe Node
aStarLoop queue =
    case PriorityQueue.head queue of
        Just current ->
            if final current then
                Just current
            else
                let
                    restQueue = PriorityQueue.tail queue
                    children = generateChildren current
                    newQueue = children |> List.foldr PriorityQueue.insert restQueue
                in
                    aStarLoop newQueue
        _ -> Nothing

resolutionMethod : DisjunctSet -> Maybe (List LogEntry)
resolutionMethod startingSet =
    let
        startNode = {disjuncts = startingSet, gScore = 0, log = []}
        emptyQueue = PriorityQueue.empty (\ node -> node.gScore + hScore node)
        initialQueue = PriorityQueue.insert startNode emptyQueue
    in
    aStarLoop initialQueue
        |> Maybe.map (\ finalNode -> finalNode.log)

