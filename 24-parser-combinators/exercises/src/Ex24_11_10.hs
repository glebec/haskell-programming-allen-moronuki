module Ex24_11_10 where

import Control.Applicative
import Text.Trifecta

-- 10. Write a parser for DOT

{-

http://www.graphviz.org/doc/info/lang.html

     graph : [ STRICT ] (GRAPH | DIGRAPH) [ ID ] '{' stmt_list '}'
 stmt_list : [ stmt [ ';' ] stmt_list ]
      stmt : node_stmt
           | edge_stmt
           | attr_stmt
           | ID '=' ID
           | subgraph
 attr_stmt : (GRAPH | NODE | EDGE) attr_list
 attr_list : '[' [ a_list ] ']' [ attr_list ]
    a_list : ID '=' ID [ (';' | ',') ] [ a_list ]
 edge_stmt : (node_id | subgraph) edgeRHS [ attr_list ]
   edgeRHS : edgeop (node_id | subgraph) [ edgeRHS ]
 node_stmt : node_id [ attr_list ]
   node_id : ID [ port ]
      port : ':' ID [ ':' compass_pt ]
           | ':' compass_pt
  subgraph : [ SUBGRAPH [ ID ] ] '{' stmt_list '}'
compass_pt : (n | ne | e | se | s | sw | w | nw | c | _)

The keywords node, edge, graph, digraph, subgraph, and strict are case-
independent. Note also that the allowed compass point values are not keywords,
so these strings can be used elsewhere as ordinary identifiers and, conversely,
the parser will actually accept any identifier.

An ID is one of the following:

- Any string of alphabetic ([a-zA-Z\200-\377]) characters, underscores ('_') or
  digits ([0-9]), not beginning with a digit;
- a numeral [-]?(.[0-9]+ | [0-9]+(.[0-9]*)? );
- any double-quoted string ("...") possibly containing escaped quotes (\")1;
- an HTML string (<...>).

An ID is just a string; the lack of quote characters in the first two forms is
just for simplicity. There is no semantic difference between abc_2 and "abc_2",
or between 2.34 and "2.34". Obviously, to use a keyword as an ID, it must
be quoted.

-}
