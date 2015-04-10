module Grafo (Grafo, vacio, nodos, vecinos, agNodo, sacarNodo, agEje, lineal, union, clausura) where

import Data.Maybe
import qualified Data.List
data Grafo a = G [a] (a -> [a])

instance (Show a) => Show (Grafo a) where
    show (G n e) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (e x) ++ "\n") n) ++ "]"

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
sacarNodo :: a -> Grafo a -> Grafo a
sacarNodo n (G ns f) = G (filter (/=n) ns) (\x -> if x/=n
                        then filter (/=n) (f x)
                        else []) 

-- Ejercicio 6
agEje :: (a,a) -> Grafo a -> Grafo a
agEje (n1,n2) (G ns f) = G ns (\n ->  if n==n1
                  then (union [n2] (f n))
                  else f n)

-- Ejercicio 7
-- TODO: Revisar. Creo que elemIndex se usa elemIndex n ns
-- Tambien, me parece que hay que usar fromJust con elemIndex (http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Maybe.html#t:Maybe)
lineal :: (Eq a) => [a] -> Grafo a
lineal ns = G ns (\n -> if (elem n ns) && (n /= (last ns))
            then [ ns !! (fromJust (Data.List.elemIndex n ns) +1)]
            else [])

-- Ejercicio 8
union :: (Eq a) => Grafo a -> Grafo a -> Grafo a
union (G ns1 f1) (G ns2 f2) = G (Data.List.union ns1 ns2) (\n -> Data.List.union (f1 n) (f2 n))
--TODO Ver q pasa si n no pertenecia a uno de los dos

-- Ejercicio 9
clausura :: Grafo a -> Grafo a
clausura = undefined















