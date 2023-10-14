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

hScore : Node -> Int -> Int
hScore node maxDepth =
    node.disjuncts
        |> DisjunctSet.toList
        |> List.map Disjunct.size
        |> List.minimum
        |> Maybe.withDefault maxDepth
    -- minPseudoResolutionSteps node.disjuncts maxDepth

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
aStarLoop queue maxDepth =
    case PriorityQueue.head queue of
        Just current ->
            if List.length current.log > maxDepth then
                Nothing
            else if final current then
                Just current
            else
                let
                    restQueue = PriorityQueue.tail queue
                    children = generateChildren current
                    -- _ = Debug.log "logs" (List.map (\ node -> node.log) children)
                    newQueue = children |> List.foldr PriorityQueue.insert restQueue
                in
                    aStarLoop newQueue maxDepth
        _ -> Nothing

resolutionMethod : DisjunctSet -> Int -> Maybe (List (String, String))
resolutionMethod startingSet maxDepth =
    let
        startNode = {disjuncts = startingSet, gScore = 0, log = []}
        emptyQueue = PriorityQueue.empty (\ node -> node.gScore + hScore node maxDepth)
        initialQueue = PriorityQueue.insert startNode emptyQueue
    in
    aStarLoop initialQueue maxDepth
        |> Maybe.map (\ finalNode -> finalNode.log)

-- bfsLoop : List Node -> Int -> Maybe Node
-- bfsLoop queue maxDepth =
--     case List.head queue of
--         Just current ->
--             if List.length current.log > maxDepth then
--                 Nothing
--             else if final current then
--                 Just current
--             else
--                 let
--                     restQueue = Maybe.withDefault [] (List.tail queue)
--                     children = generateChildren current
--                     -- _ = Debug.log "logs" (List.map (\ node -> node.log) children)
--                     newQueue =  List.append children restQueue
--                 in
--                     bfsLoop newQueue maxDepth
--         _ -> Nothing
