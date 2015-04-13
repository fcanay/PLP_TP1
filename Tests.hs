import Grafo
import Tipos
import Lomoba
import Parser
import Test.HUnit
import Data.List
import Data.Maybe

-- Ejemplos que se me fueron ocurriendo para testear
-- Figura 1 del tp
-- K (agEje (1,2) $ agEje (1,3) $ agNodo 3 $ agNodo 2 $ agNodo 1 $ vacio) (\x -> if x == "p" then [1] else (if x == "q" then [2,3] else (if x == "r" then [2] else [0])))

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [
	"parser" ~: testsParser,
	"grafo" ~: testsGrafo
	]

testsParser = test [
	(Var "p") 						~=? (parse "p"),
	(And (Var "p") (Var "q")) 		~=? (parse "p && q"),
	(Or (Var "p") (Var "q")) 		~=? (parse "p || q"),
	(Or (Not (Var "p")) (Var "q"))	~=? (parse "!p || q"),
	(And (D (Var "p")) (Var "q")) 	~=? (parse "<>p && q"),
	(And (B (Var "p")) (Var "q")) 	~=? (parse "[]p && q"),
	(D (And (Var "p") (Var "q"))) 	~=? (parse "<>(p && q)"),
	(B (And (Var "p") (Var "q"))) 	~=? (parse "[](p && q)")]

testsGrafo = test [
	[1] ~~? (nodos (agNodo 1 vacio)),
	[1,2] ~~? (nodos (agNodo 2 (agNodo 1 vacio))),
	[] ~~? (nodos (sacarNodo 1 (agNodo 1 vacio))),
	[1,2] ~~? (nodos (union (agNodo 1 vacio) (agNodo 2 vacio))),
	[1,2] ~~? (nodos (union (agNodo 1 vacio) (agNodo 2 (agNodo 1 vacio)))),
	[1,2,3,4] ~~? (nodos (clausura(lineal[1..4]))),
	[1,2,3,4] ~~? (vecinos (clausura(lineal[1..4])) 1),
	[2] ~~? (vecinos (union (agNodo 1 vacio) (agEje (1,2) (agNodo 2 (agNodo 1 vacio)))) 1),
	[] ~~? (vecinos (union (agNodo 1 vacio) (agEje (1,2) (agNodo 2 (agNodo 1 vacio)))) 2)
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
