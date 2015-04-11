module Grafo (Grafo, vacio, nodos, vecinos, agNodo, sacarNodo, agEje, lineal, union, clausura) where

import Data.Maybe
import qualified Data.List
data Grafo a = G [a] (a -> [a])

instance (Show a) => Show (Grafo a) where
    show (G n e) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (e x) ++ "\n") n) ++ "]"

instance Eq a=> Eq (Grafo a) where  
        G n1 e1 == G n2 e2 = n1 == n2 &&
							foldr (\x res -> e1 x == e2 x && res) True n1

-- ---------------------------------Sección 3--------- Grafos ---------------------------
--TODO tener en cuenta el comportamiento de la f cunado el nodo no pertence al grafo
-- Ejercicio 1
vacio :: Grafo a
vacio = G [] (const [])

-- Ejercicio 2
nodos :: Grafo a -> [a]
nodos (G ns _) = ns

-- Ejercicio 3
vecinos :: Grafo a -> a -> [a]
vecinos (G _ f) n = f n

-- Ejercicio 4
agNodo :: a -> Grafo a -> Grafo a
agNodo n (G ns f) = G (n:ns) f

-- Ejercicio 5
sacarNodo ::(Eq a) =>  a -> Grafo a -> Grafo a
sacarNodo n (G ns f) = G (filter (/=n) ns) (\x ->   if x/=n
                                                    then filter (/=n) (f x)
                                                    else [])    

-- Ejercicio 6
agEje :: (Eq a) => (a,a) -> Grafo a -> Grafo a
agEje (n1,n2) (G ns f) =    if (elem n2 ns) then
                                G ns (\n -> if n == n1 then (Data.List.union [n2] (f n)) else f n)
                            else
                                G ns f

-- Ejercicio 7
lineal :: (Eq a) => [a] -> Grafo a
lineal ns = G ns (\n -> if (elem n ns) && (n /= (last ns))
                        then [ ns !! (fromJust (Data.List.elemIndex n ns) + 1)]
                        else [])
-- Ejercicio 8
union :: (Eq a) => Grafo a -> Grafo a -> Grafo a
union (G ns1 f1) (G ns2 f2) = G (Data.List.union ns1 ns2) (\n -> Data.List.union (f1 n) (f2 n))
--TODO Ver q pasa si n no pertenecia a uno de los dos

-- Ejercicio 9
clausura :: (Eq a) => Grafo a -> Grafo a
clausura (G ns f) = foldr (\x g -> unPasoClausura g) (G ns f) ns 
--puntofijo kleene(G ns f)  

unPasoClausura :: (Eq a) => Grafo a -> Grafo a
unPasoClausura (G ns f) = G ns (\n -> Data.List.union [n] (Data.List.union (f n) (foldr (\x rec ->  if elem x (f n) 
                                                                                                    then Data.List.union (f x) rec 
                                                                                                    else rec) 
                                                                                                    [] ns)))

clausuraSanti :: (Eq a) => Grafo a -> Grafo a
clausuraSanti = puntoFijo unPasoClausura
								
puntoFijo :: (Eq a) => (a -> a) -> a -> a
puntoFijo f = (\n -> buscoPuntoFijo f n)

--TODO Preguntar si esta es la solucion (tiene recursion explicita)
buscoPuntoFijo :: (Eq a) => (a -> a) -> a -> a
buscoPuntoFijo f x = 	if f x == x
						then x
						else buscoPuntoFijo f (f x)

--puntofijoF :: (Eq a) => (a -> a) -> (a -> a)
--puntofijoF f = (\n -> foldr id [1..])









