module Tipos where

import Grafo

type Mundo = Integer
type Prop = String
data Modelo = K (Grafo Mundo) (Prop -> [Mundo])

-- No tengo idea como mostraria la funcion Prop -> [Mundo] (hay infinitas Prop)
instance Show Modelo where  
    show (K g f) = "[\n" ++ "Grafo: " ++ show g ++ "\n]"

data Exp = Var Prop | Not Exp | Or Exp Exp | And Exp Exp | D Exp | B Exp deriving (Show, Eq)
