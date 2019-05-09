module Syntax where

import Data.List

-- Pueden definir cualquier otra funcion que consideren necesaria
-- o reutilizar cualquiera de las que definimos en proyectos anteriores.

type Ind = Integer

type Nombre = String

type Subst = [(Ind, Term)]

data Term = V Ind | F Nombre [Term] deriving(Show,Eq)

-- Tipo de dato que representa literalese
--Pr es predicado
data Lit = TrueF
         | FalseF
         | Pr Nombre [Term]
         | Eq Term Term deriving (Show, Eq)

varT :: Term -> [Ind]
varT term = case term of
  V x -> [x]
  F _ l -> nub (concat [varT t | t <- l])


--aplicacion de sustitucion para un solo termino.
apsubT :: Term -> Subst -> Term
apsubT t sus = case t of
  V x -> case sus of
    [] -> V x
    (v, t2):xs -> if x == v
                  then t2
                  else apsubT (V x) xs
  F f lt -> F f [apsubT t sus | t <- lt]

--aplicaacion de sustitucion para literales.
apsubL :: Lit -> Subst -> Lit
apsubL lit sust = case lit of
  TrueF -> TrueF
  FalseF -> FalseF
  Pr p lt -> Pr p [apsubT t sust | t <- lt]
  Eq t1 t2 -> Eq (apsubT t1 sust) (apsubT t2 sust)


--aplicacion de sustitucion para una lista de terminos.
apsublst :: [Term] -> Subst -> [Term]
apsublst lista sus = map (`apsubT` sus) lista





