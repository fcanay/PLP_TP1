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
--visibilidad :: Exp -> Integer
--visibilidad = foldExp (const 0) id max max (+1) (+1)

-- Ejercicio 12
extraer :: Exp -> [Prop]
extraer = foldExp (:[]) id Data.List.union Data.List.union id id

-- Ejercicio 13
eval :: Modelo -> Mundo -> Exp -> Bool
eval mod m exp = eval' mod exp m

--Santi: Por que el 'm' (de mundo) no lo pones a la izq y lo pedis en todas las lambdas?
eval':: Modelo -> Exp -> Mundo -> Bool
eval' (K (G ns f) fProp) exp = foldExp  (\p m -> elem m (fProp p))  
                                        (\f1 -> (\m -> not(f1 m)))  
                                        (\f1 f2 -> (\m -> (f1 m) || (f2 m))) 
                                        (\f1 f2 -> (\m -> (f1 m) && (f2 m))) 
                                        (\f1 m -> any f1 (f m))
                                        (\f1 m -> all f1 (f m))
                                        exp
										
--TODO acortar la funcion anterior con composicion

-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn exp (K (G ns f) fProp) = [ n | n <- ns, eval m n exp ]
                                where m = K (G ns f) fProp

-- Ejercicio 15
quitar :: Exp -> Modelo -> Modelo
quitar exp m = foldr quitarMundo m (valeEn exp m)

--Santi: Quisiste poner 'sacarNodo n g'?
quitarMundo :: Mundo -> Modelo -> Modelo
quitarMundo n (K g fProp) = K (sacarNodo n g) (\p -> filter (/=n) (fProp p))

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto (K (G ns f) fProp) exp = all (eval' m exp) ns
                                      where m = (K (G ns f) fProp)