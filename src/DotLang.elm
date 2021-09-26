module DotLang exposing
    ( fromString
    , ID
    , NodeId(..)
    , dot, node
    )

{-| Parse DOT Language to directed graph in Elm.
Only parses a 'digraph'.
Take a look at the grammar <https://www.graphviz.org/doc/info/lang.html>

@docs fromString


# DOT Components

@docs ID

# Stmt Components

@docs NodeId, Subgraph


# Internal

@docs dot

-}

import Dict exposing (Dict)
import DoubleQuoteString as DQS
import Parser exposing (..)
import Graph exposing (..)
import Set
import Iter exposing (Iter)
import Tree

node : Graph.NodeId -> String -> Node String
node nid label =
    { id = nid
    , label = label
    }

edge : Graph.NodeId -> Graph.NodeId -> Edge String
edge from to =
    { from = from
    , to = to
    , label = "depends on"
    }


{-| Parse a DOT string.

    fromString "graph {}" == Ok (Dot Graph Nothing [])

-}
fromString : String -> Result (List Parser.DeadEnd) (Graph String String)
fromString =
    Parser.run dot

type alias NodesAndEdge = (Node String, Node String, Edge String)

type alias Dependencies =
    { graph : (Graph String String)
    , tree: Tree.Node String
    }

{-| The core `Parser`, in case you want to embed it in some other parser.
-}
-- TODO result: combine Graph, Graph.Tree(?) and Tree/TreeView
dot : Parser (Graph String String)
dot =
    succeed
        (\nodesAndEdgeList -> 
            let _ = Debug.log "result " nodesAndEdgeList
            in
            Graph.fromNodesAndEdges (List.foldl aggNodes [] nodesAndEdgeList) (List.foldl aggEdges [] nodesAndEdgeList)
        )
        |. spacing
        |. symbol "digraph"
        |. spacing
        |. maybeParse id
        |. spacing
        |= stmtList 

aggNodes : NodesAndEdge -> List (Node String) -> List (Node String)
aggNodes (n1, n2, _) nodes =
    n1 :: n2 :: nodes

aggEdges : NodesAndEdge -> List (Edge String) -> List (Edge String)
aggEdges (_, _, e) edges =
    e :: edges

type alias S = (Dict String Int, Iter Int Int)

startIter = Tuple.first (Iter.step Iter.numbers)

stmtList : Parser (List NodesAndEdge)
stmtList =
    let
        help : (S, List NodesAndEdge) -> Parser (Step (S, List NodesAndEdge) (S, List NodesAndEdge))
        help ( (d, it), nodesAndEdge) =
            oneOf
                [ succeed (\((dnext, itnext), nae) -> Loop ((dnext, itnext), (nae :: nodesAndEdge)))
                    |= statement (d, it)
                    |. spacing
                    |. oneOf
                        [ symbol ";"
                        , succeed ()
                        ]
                    |. spacing
                , succeed ()
                    |> map (\_ -> Done ((d, it), nodesAndEdge))
                ]
    in
    succeed (\(_, r) -> r)
        |. symbol "{"
        |. spacing
        |= loop ((Dict.empty, startIter), []) help
        |. spacing
        |. symbol "}"

-- TODO pass Pair (Dict with key: label, value: NodeId, Iter.numbers)
statement : S -> Parser (S, NodesAndEdge)
statement (d, it) =
    succeed (\from to ->
        let (it2, idFrom) = case (Dict.get from d) of
                                Nothing ->
                                            let (itnext, val) = Iter.step it
                                                n = case val of
                                                        Nothing -> 42
                                                        Just i -> i
                                            in (itnext, n)
                                Just n -> (it, n)

            (it3, idTo) = case (Dict.get to d) of
                                Nothing ->
                                            let (itnext, val) = Iter.step it2
                                                n = case val of
                                                        Nothing -> 42
                                                        Just i -> i
                                            in (itnext, n)
                                Just n -> (it2, n)
            dnext = Dict.insert to idTo (Dict.insert from idFrom d)
        in
        ((dnext, it3), (node idFrom from, node idTo to, edge idFrom idTo))
    )
    |. spacing
    |= id
    |. spacing
    |. symbol "->"
    |. spacing
    |= id

unquotedVariable : Parser String
unquotedVariable =
    variable
        { start = \c -> Char.isAlpha c || c == '_'
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList []
        }

{-| `NodeId` describes the `ID` of a vertex. Potentially, it has a `Port` which
describes where edges can attach to the vertex.
-}
type NodeId
    = NodeId ID


nodeId : Parser NodeId
nodeId =
    succeed NodeId
        |= id


{-| The identifier for a vertex.
-}
type alias ID = String



maybeParse : Parser a -> Parser (Maybe a)
maybeParse parser =
    oneOf
        [ map Just parser
        , succeed Nothing
        ]

repeat : Parser a -> Parser (List a)
repeat parser =
    let
        help : List a -> Parser (Step (List a) (List a))
        help revStmts =
            oneOf
                [ succeed (\stmt -> Loop (stmt :: revStmts))
                    |= parser
                , succeed ()
                    |> map (\_ -> Done (List.reverse revStmts))
                ]
    in
    loop [] help


comment : Parser ()
comment =
    oneOf
        [ lineComment "//"
        , multiComment "/*" "*/" NotNestable
        , lineComment "#"
        ]



spacing : Parser ()
spacing =
    let
        isSpace =
            \c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t'
    in
    succeed ()
        |. (repeat <|
                oneOf
                    [ succeed ()
                        |. chompIf isSpace
                        |. chompWhile isSpace
                    , comment
                    ]
           )


id : Parser ID
id = DQS.string
