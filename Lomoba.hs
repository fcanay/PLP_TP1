module Lomoba where
import Grafo
import Tipos


-- ---------------------------------Sección 6--------- Lomoba ---------------------------

-- Ejercicio 10
--foldExp :: (Prop -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> (b -> b) -> Exp -> b
foldExp fVar fNot fOr fAnd fD fB ex = let rec be foldExp fVar fNot fOr fAnd fD fB
                                      case ex of  Var p         -> fVar p
                                                  Not exr       -> fNot rec exr
                                                  Or  exr1 exr2 -> fOr  (rec exr1) (rec exr2) 
                                                  And exr1 exr2 -> fAnd (rec exr1) (rec exr2)
                                                  D   exr1      -> fD rec exr
                                                  B   exr1      -> fB rec exr
-- Ejercicio 11   
visibilidad :: Exp -> Integer
visibilidad = foldExp (const 0) (id) (max) (max) (+1) (+1)

-- Ejercicio 12
extraer :: Exp -> [Prop]
extraer = undefined

-- Ejercicio 13
eval :: Modelo -> Mundo -> Exp -> Bool
eval = undefined

-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn = undefined

-- Ejercicio 15
quitar :: Exp -> Modelo -> Modelo
quitar = undefined

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto = undefined

