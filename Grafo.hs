module Grafo (Grafo, vacio, nodos, vecinos, agNodo, sacarNodo, agEje, lineal, union, clausura) where

data Grafo a = G [a] (a -> [a])

instance (Show a) => Show (Grafo a) where
	show (G n e) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (e x) ++ "\n") n) ++ "]"


-- ---------------------------------Sección 3--------- Grafos ---------------------------

-- Ejercicio 1
vacio :: Grafo a
vacio = G [] (const [])

-- Ejercicio 2
nodos :: Grafo a -> [a]
nodos G ns _ = ns

-- Ejercicio 3
vecinos :: Grafo a -> a -> [a]
vecinos G _ f n = f a

-- Ejercicio 4
agNodo :: a -> Grafo a -> Grafo a
agNodo n G ns f = G (n:ns) f

-- Ejercicio 5
sacarNodo :: a -> Grafo a -> Grafo a
sacarNodo n G ns f = G (filter (==n) ns) (\x -> filter (==n) (f x)) 

-- Ejercicio 6
agEje :: (a,a) -> Grafo a -> Grafo a
agEje (n1,n2) G ns f = G ns (\n -> if n==n1 then (n2:(f n)) otherwise f n)

-- Ejercicio 7
lineal :: [a] -> Grafo a
lineal = undefined

-- Ejercicio 8
union :: Grafo a -> Grafo a -> Grafo a
union = undefined

-- Ejercicio 9
clausura :: Grafo a -> Grafo a
clausura = undefined





