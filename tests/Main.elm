module Main exposing (testFromString)

import DotLang exposing (..)
import Expect
import Graph exposing (..)
import Test exposing (..)
import DotLang exposing (..)



-- examples from https://graphs.grevian.org/example


psg : String
psg =
    -- https://graphviz.gitlab.io/_pages/Gallery/directed/psg.html
    "##\"I made a program to generate dot files representing the LR(0) state graph along with computed LALR(1) lookahead for an arbitrary context-free grammar, to make the diagrams I used in this article: http://blog.lab49.com/archives/2471. The program also highlights errant nodes in red if the grammar would produce a shift/reduce or reduce/reduce conflict -- you may be able to go to http://kthielen.dnsalias.com:8082/ to produce a graph more to your liking\". Contributed by Kalani Thielen.\n\n##Command to get the layout: \"dot -Gsize=10,15 -Tpng thisfile > thisfile.png\"\n\ndigraph g {\n  graph [fontsize=30 labelloc=\"t\" label=\"\" splines=true overlap=false rankdir = \"LR\"];\n  ratio = auto;\n  \"state0\" [ style = \"filled, bold\" penwidth = 5 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #0</font></td></tr><tr><td align=\"left\" port=\"r0\">&#40;0&#41; s -&gt; &bull;e $ </td></tr><tr><td align=\"left\" port=\"r1\">&#40;1&#41; e -&gt; &bull;l '=' r </td></tr><tr><td align=\"left\" port=\"r2\">&#40;2&#41; e -&gt; &bull;r </td></tr><tr><td align=\"left\" port=\"r3\">&#40;3&#41; l -&gt; &bull;'*' r </td></tr><tr><td align=\"left\" port=\"r4\">&#40;4&#41; l -&gt; &bull;'n' </td></tr><tr><td align=\"left\" port=\"r5\">&#40;5&#41; r -&gt; &bull;l </td></tr></table>> ];\n  \"state1\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #1</font></td></tr><tr><td align=\"left\" port=\"r3\">&#40;3&#41; l -&gt; &bull;'*' r </td></tr><tr><td align=\"left\" port=\"r3\">&#40;3&#41; l -&gt; '*' &bull;r </td></tr><tr><td align=\"left\" port=\"r4\">&#40;4&#41; l -&gt; &bull;'n' </td></tr><tr><td align=\"left\" port=\"r5\">&#40;5&#41; r -&gt; &bull;l </td></tr></table>> ];\n  \"state2\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #2</font></td></tr><tr><td align=\"left\" port=\"r4\">&#40;4&#41; l -&gt; 'n' &bull;</td><td bgcolor=\"grey\" align=\"right\">=$</td></tr></table>> ];\n  \"state3\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #3</font></td></tr><tr><td align=\"left\" port=\"r5\">&#40;5&#41; r -&gt; l &bull;</td><td bgcolor=\"grey\" align=\"right\">=$</td></tr></table>> ];\n  \"state4\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #4</font></td></tr><tr><td align=\"left\" port=\"r3\">&#40;3&#41; l -&gt; '*' r &bull;</td><td bgcolor=\"grey\" align=\"right\">=$</td></tr></table>> ];\n  \"state5\" [ style = \"filled\" penwidth = 1 fillcolor = \"black\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"black\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #5</font></td></tr><tr><td align=\"left\" port=\"r0\"><font color=\"white\">&#40;0&#41; s -&gt; e &bull;$ </font></td></tr></table>> ];\n  \"state6\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #6</font></td></tr><tr><td align=\"left\" port=\"r1\">&#40;1&#41; e -&gt; l &bull;'=' r </td></tr><tr><td align=\"left\" port=\"r5\">&#40;5&#41; r -&gt; l &bull;</td><td bgcolor=\"grey\" align=\"right\">$</td></tr></table>> ];\n  \"state7\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #7</font></td></tr><tr><td align=\"left\" port=\"r1\">&#40;1&#41; e -&gt; l '=' &bull;r </td></tr><tr><td align=\"left\" port=\"r3\">&#40;3&#41; l -&gt; &bull;'*' r </td></tr><tr><td align=\"left\" port=\"r4\">&#40;4&#41; l -&gt; &bull;'n' </td></tr><tr><td align=\"left\" port=\"r5\">&#40;5&#41; r -&gt; &bull;l </td></tr></table>> ];\n  \"state8\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #8</font></td></tr><tr><td align=\"left\" port=\"r1\">&#40;1&#41; e -&gt; l '=' r &bull;</td><td bgcolor=\"grey\" align=\"right\">$</td></tr></table>> ];\n  \"state9\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #9</font></td></tr><tr><td align=\"left\" port=\"r2\">&#40;2&#41; e -&gt; r &bull;</td><td bgcolor=\"grey\" align=\"right\">$</td></tr></table>> ];\n  state0 -> state5 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"e\" ];\n  state0 -> state6 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"l\" ];\n  state0 -> state9 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"r\" ];\n  state0 -> state1 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'*'\" ];\n  state0 -> state2 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'n'\" ];\n  state1 -> state1 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'*'\" ];\n  state1 -> state4 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"r\" ];\n  state1 -> state2 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'n'\" ];\n  state1 -> state3 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"l\" ];\n  state6 -> state7 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'='\" ];\n  state7 -> state8 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"r\" ];\n  state7 -> state1 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'*'\" ];\n  state7 -> state2 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'n'\" ];\n  state7 -> state3 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"l\" ];\n}\n"


testFromString : Test
testFromString =
    let
        edge : Graph.NodeId -> Graph.NodeId -> Edge String
        edge a b =
            { from = a
            , to = b
            , label = "depends on"
            }
    in
    describe "Dot Lang Parser"
        [ test "parsing simple digraph" <|
            \_ ->
                Expect.equal
                    (DotLang.fromString
                        (String.join "\n"
                            [ "digraph \"module\" {"
                            , "    \"a\" -> \"b\";"
                            , "    \"b\" -> \"c\";"
                            , "}"
                            ]
                        )
                    )
                    (Ok
                        (fromNodesAndEdges
                            [ node 1 "a"
                            , node 2 "b"
                            , node 3 "c"
                            ]
                            [ edge 1 2
                            , edge 2 3
                            ]
                        )
                    )
        ]
