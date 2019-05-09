module AlgMM where 

import Data.List
import Syntax

--Sustitucion simple.
simpSus :: Subst -> Subst
simpSus sus = [(x, t) | (x, t) <- sus, V x /= t]

--Composicion de sustituciones.
compSus :: Subst -> Subst -> Subst
compSus s1 s2 = zs ++ ws 
        where zs = simpSus [(x, apsubT t s2) | (x, t) <- s1]
              ws = [(x, t) | (x, t) <- s2, not (elem x vs1)]
              vs1 = fst(unzip s1)



hazPares :: [a] -> [(a, a)]
hazPares l = case l of
  [] -> [] 
  x:y:xs -> (x, y):(hazPares (y:xs))
  x:xs -> []

--unifica dos literales y devuelve una sustitucion
unificaLit :: Lit -> Lit -> Subst
unificaLit TrueF _ = []
unificaLit FalseF _ = []
unificaLit (Pr n1 (x:xs)) (Pr n2 (y:ys)) = if n1 == n2 && length (x:xs) == length (y:ys)
                                         then compSus (unifica x y) (unificaC_aux (zip xs ys))
                                         else error "No se puede unificar"


sust_G :: [Lit] -> Subst -> [Lit]
sust_G lista sust = map (`apsubL` sust) lista 


--Unifica un conjunto de literales y devuelve una sustitucion
mmE :: [Lit] -> Subst
mmE lista = mmEaux(nub(hazPares lista)) 


--funcion auxiliar para la unificacion del conjunto de literales
mmEaux:: [(Lit,Lit)] -> Subst
mmEaux par = case par of 
   [] -> []
   (lt1,lt2):lp -> case (lt1,lt2) of
      (FalseF, _) -> mmEaux lp
      (TrueF, _) -> mmEaux lp
      (Pr n1 xs, Pr n2 ys) -> if n1 == n2 && length xs == length ys
                              then compSus (unificaC_aux((zip xs ys))) (mmEaux lp)
                              else error "No se puede unificar"


--funcion que toma dos terminos y devuelve una sustitución.
unifica :: Term -> Term -> Subst
unifica (V x) (V y) = if x == y
                      then []
                      else [(x, V y)]
unifica (V x) t = if elem x (varT t)
                  then []
                  else [(x,t)]
unifica t (V x) = unifica (V x) t
unifica (F f (x:xs)) (F g (y:ys)) = if f == g && length (x:xs) == length (y:ys)
                            then compSus (unifica x y) (unificaC_aux (zip xs ys))
                            else error "LOL"



--unifica dos listas de terminos devolviendo una lista con las sustituciones.
unificaL :: [Term] -> [Term] -> [Subst]
unificaL (x:xs) (y:ys) = [unifica x y] ++ (unificaL xs ys)
unificaL _ _ = []


--Unifica un conjunto de terminos
unificaC :: [Term] -> [Subst]
unificaC lista = [unificaC_aux(nub(hazPares lista))]

--Metodo auxiliar para la unificacion de conjuntos.
unificaC_aux ::[(Term, Term)] -> Subst
unificaC_aux pares = case pares of
  [] -> []
  (t1, t2):lp -> case (t1, t2) of
    (F f lt1, F g lt2) -> if f == g && length lt1 == length lt2
                          then unificaC_aux ((zip lt1 lt2) ++ lp) --DESC
                          else error "Imposible Unificar" -- DFALLA
    (V x , V y) -> if x == y
                   then unificaC_aux lp
                   else compSus d (unificaC_aux lps) --SUST (Cuando t es variable)
                            where d = [(x, V y)]
                                  lps = [( apsubT t1 d, apsubT t2 d) | (t1, t2) <- lp]