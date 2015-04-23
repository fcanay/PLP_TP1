import Grafo
import Tipos
import Lomoba
import Parser
import Test.HUnit
import qualified Data.List
import Data.Maybe

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [
    "parser" ~: testsParser,
    "grafo" ~: testsGrafo,
    "lomoba" ~: testsLomoba
    ]

testsParser = test [
    (Var "p")                      ~=? (parse "p"),
    (And (Var "p") (Var "q"))      ~=? (parse "p && q"),
    (Or (Var "p") (Var "q"))       ~=? (parse "p || q"),
    (Or (Not (Var "p")) (Var "q")) ~=? (parse "!p || q"),
    (And (D (Var "p")) (Var "q"))  ~=? (parse "<>p && q"),
    (And (B (Var "p")) (Var "q"))  ~=? (parse "[]p && q"),
    (D (And (Var "p") (Var "q")))  ~=? (parse "<>(p && q)"),
    (B (And (Var "p") (Var "q")))  ~=? (parse "[](p && q)")
    ]

testsGrafo = test [
    [1]       ~~? (nodos (agNodo 1 vacio)),
    [1,2]     ~~? (nodos (agNodo 2 (agNodo 1 vacio))),
    []        ~~? (nodos (sacarNodo 1 (agNodo 1 vacio))),
    [1,2]     ~~? (nodos (union (agNodo 1 vacio) (agNodo 2 vacio))),
    [1,2]     ~~? (nodos (union (agNodo 1 vacio) (agNodo 2 (agNodo 1 vacio)))),
    [1,2,3,4] ~~? (nodos (clausura(lineal[1..4]))),
    [2]       ~~? (vecinos (lineal[1..4]) 1),
    [1,2,3,4] ~~? (vecinos (clausura(lineal[1..4])) 1),
    [2]       ~~? (vecinos (union (agNodo 1 vacio) (agEje (1,2) (agNodo 2 (agNodo 1 vacio)))) 1),
    []        ~~? (vecinos (union (agNodo 1 vacio) (agEje (1,2) (agNodo 2 (agNodo 1 vacio)))) 2),
    [2,4]     ~~? (vecinos (sacarNodo 3 (clausura(lineal[1..4]))) 2)
    ]


    
k = K (lineal [1]) (\x -> if (x == "p") || (x == "q") then [1] else [])
k1 = K (agEje (1,2) $ agEje (1,3) $ agNodo 3 $ agNodo 2 $ agNodo 1 $ vacio) (\x -> if x == "p" then [2,3] else (if x == "q" then [3] else[]))
figura1 = K (agEje (1,2) $ agEje (1,3) $ agNodo 3 $ agNodo 2 $ agNodo 1 $ vacio) (\x -> if x == "p" then [1] else (if x == "q" then [2,3] else (if x == "r" then [2] else [0])))
k2 = K (clausura $ agEje (3,1) $ lineal [1..3])  (\x -> if x == "p" then [1,2,3] else (if x == "q" then [3] else[]))

testsLomoba = test [
    0 ~=? (visibilidad (parse "p")),
    1 ~=? (visibilidad (parse "<>p")),
    2 ~=? (visibilidad (parse "<>!<>p")),
    2 ~=? (visibilidad (parse "<>(<>p) || <>(<>q)")),
    3 ~=? (visibilidad (parse "<>(<>p || <><>q)")),
    3 ~=? (visibilidad (parse "[](<>p && <>[]q)")),

    ["p"]      ~~? (extraer (parse "p")),
    ["p"]      ~~? (extraer (parse "<>p")),
    ["p"]      ~~? (extraer (parse "<>!<>p")),
    ["p", "q"] ~~? (extraer (parse "<><>p || <><>q")),
    ["p", "q"] ~~? (extraer (parse "<>(<>p || <><>q)")),
    ["p", "q"] ~~? (extraer (parse "[](<>p && <>[]q)")),

    True  ~=? (eval k 1  (parse "p")),
    False ~=? (eval k 1  (parse "!p")),
    False ~=? (eval k 1  (parse "r")),
    True  ~=? (eval k 1  (parse "!r")),
    True  ~=? (eval k 1  (parse "p && q")),
    True  ~=? (eval k 1  (parse "p || q")),
    True  ~=? (eval k 1  (parse "p || r")),
    False ~=? (eval k 1  (parse "r || j")),

    True  ~=? (eval k1 1  (parse "<>p")),
    True  ~=? (eval k1 1  (parse "[]p")),
    True  ~=? (eval k1 1  (parse "<>q")),
    False ~=? (eval k1 1  (parse "[]q")),
    False ~=? (eval k1 1  (parse "<>r")),
    False ~=? (eval k1 1  (parse "[]r")),

    [1]     ~~? (valeEn (parse "p") figura1),
    [2,3]   ~~? (valeEn (parse "!p") figura1),
    [1]     ~~? (valeEn (parse "<>q") figura1),
    [2,3]   ~~? (valeEn (parse "[]r") figura1),
    [2]     ~~? (valeEn (parse "r || j") figura1),
    [2,3]   ~~? (valeEn (parse "[](r || j)") figura1),
    []      ~~? (valeEn (parse "<>!q") figura1),
    []      ~~? (valeEn (parse "<><>!q") figura1),
    [2,3]   ~~? (valeEn (parse "[]!q") figura1),
    [1,2,3] ~~? (valeEn (parse "[][]!q") figura1),

    [2,3] ~~? (valeEn (parse "q") (quitar (parse "p") figura1)),
    []    ~~? (valeEn (parse "dameFalse") (quitar (parse "[][]!q") figura1)),
    []    ~~? (valeEn (parse "r") (quitar (parse "[]q") figura1)),

    True  ~=? (cierto k2 (parse "p")),
    False ~=? (cierto k2 (parse "q")),
    True  ~=? (cierto k2 (parse "<>p")),
    True  ~=? (cierto k2 (parse "[]p")),
    True  ~=? (cierto k2 (parse "<>q")),
    False ~=? (cierto k2 (parse "[]q"))

    ]

---------------
--  helpers  --
---------------

-- idem ~=? pero sin importar el orden
(~~?) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Test
expected ~~? actual = (sort expected) ~=? (sort actual)
    where
        sort = foldl (\r e -> push r e) []
        push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)
