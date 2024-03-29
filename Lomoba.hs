module Lomoba where
import Grafo
import Tipos
import qualified Data.List

-- ---------------------------------Sección 6--------- Lomoba ---------------------------

-- Ejercicio 10
foldExp :: (Prop -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> (b -> b) -> Exp -> b
foldExp fVar fNot fOr fAnd fD fB ex = case ex of  
                                                  Var p         -> fVar p
                                                  Not exr       -> fNot (rec exr)
                                                  Or  exr1 exr2 -> fOr (rec exr1) (rec exr2) 
                                                  And exr1 exr2 -> fAnd (rec exr1) (rec exr2)
                                                  D   exr1      -> fD (rec exr1)
                                                  B   exr1      -> fB (rec exr1)
                                                  where rec = foldExp fVar fNot fOr fAnd fD fB
-- Ejercicio 11   
visibilidad :: Exp -> Integer
visibilidad = foldExp (const 0) id max max (+1) (+1)

-- Ejercicio 12
extraer :: Exp -> [Prop]
extraer = foldExp (:[]) id Data.List.union Data.List.union id id

-- Ejercicio 13
eval :: Modelo -> Mundo -> Exp -> Bool
eval mod m exp = eval' mod exp m

eval':: Modelo -> Exp -> Mundo -> Bool
eval' (K g fProp) = foldExp (\p m -> elem m (fProp p))
                            (\f1 -> (\m -> not(f1 m)))  
                            (\f1 f2 -> (\m -> (f1 m) || (f2 m))) 
                            (\f1 f2 -> (\m -> (f1 m) && (f2 m))) 
                            (\f1 m -> any f1 (vecinos g m))
                            (\f1 m -> all f1 (vecinos g m))
                                 
								
-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn exp (K g fProp) = [ n | n <- (nodos g), eval m n exp ]
                                where m = K g fProp

-- Ejercicio 15
quitar :: Exp -> Modelo -> Modelo
quitar exp m = foldr quitarMundo m (valeEn exp m)

quitarMundo :: Mundo -> Modelo -> Modelo
quitarMundo n (K g fProp) = K (sacarNodo n g) (\p -> filter (/=n) (fProp p))

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto (K g fProp) exp = all (eval' m exp) (nodos g)
                          where m = (K g fProp)