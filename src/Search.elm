module Search exposing (..)

import Disjunct
import DisjunctSet exposing (DisjunctSet)
import ResolutionStep exposing (resolvents, colapses)
import PriorityQueue exposing (PriorityQueue)
import ListHelperFunctions exposing (..)
import Heuristic exposing (minPseudoResolutionSteps)
import ResolutionStep exposing (printLogEntry)

type alias Node =
    { disjuncts : DisjunctSet
    , gScore : Int
    , log : List (String, String)
    }

-- simple heuristic that finds the minimum size of disjunct
hScore : Node -> Int
hScore node =
    minPseudoResolutionSteps node.disjuncts

generateChildren : Node -> List Node
generateChildren node = 
    let
        disjuncts = DisjunctSet.toIndexedList node.disjuncts
    in
    subsetsOf2 disjuncts
        |> List.concatMap (\ (d1, d2) -> resolvents d1 d2)
        |> List.append (disjuncts |> List.concatMap (\ d -> colapses d))
        |> List.map (\ (d, logEntry) -> {
                disjuncts = DisjunctSet.insert d node.disjuncts, 
                gScore = node.gScore + 1,
                log = List.append node.log [printLogEntry logEntry (DisjunctSet.size node.disjuncts)]
            }
        )
        |> List.filter (\ child -> child.disjuncts /= node.disjuncts) -- dont allow cycles

final : Node -> Bool
final node = 
    DisjunctSet.any Disjunct.isEmpty node.disjuncts

aStarLoop : PriorityQueue Node -> Int -> Maybe Node
aStarLoop queue step =
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
                    aStarLoop newQueue (step + 1)
        _ -> Nothing

resolutionMethod : DisjunctSet -> Maybe (List (String, String))
resolutionMethod startingSet =
    let
        startNode = {disjuncts = startingSet, gScore = 0, log = []}
        emptyQueue = PriorityQueue.empty (\ node -> node.gScore + hScore node)
        initialQueue = PriorityQueue.insert startNode emptyQueue
    in
    aStarLoop initialQueue 0
        |> Maybe.map (\ finalNode -> finalNode.log)

