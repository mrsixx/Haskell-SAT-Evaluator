module Main (main) where

-- definição recursiva de uma fórmula phi
data Prop     = T | F| Var Char | And Prop Prop | Or Prop Prop | Not Prop | Imp Prop Prop
-- função de valoração
type ValFn    =  (String, Char -> Bool)
-- tabela verdade
type Substs   = [(Char, Bool)]


-- constrói a tabela verdade
valFn2table :: ValFn -> Substs
valFn2table (ds,v) = [(d,v d) | d <- ds]

-- representa a função que reage a um dominio vazio
empty ::  ValFn
empty = ("", \x -> error "Empty val")


-- extende uma função para o passo recursivo
extend :: ValFn -> (Char, Bool) -> ValFn
extend (ds, v) (x, b) = (x:ds, w)
  where w y = if x == y then b else v y

--obtem a fórmula através da tabela
table2valFn :: Substs -> ValFn
-- table2valFnv1 tb x = (ds, v) 
--   where v = \x -> head [b | (d, b) <- tb, d == x]
--         ds = [d | (d, _) <- tb]

-- table2valFnv2 [] = empty
-- table2valFnv2 ((x, b): tb) = extend (ds, v) (x, b)
--   where (ds, v) = table2valFnv2 tb

table2valFn = foldr (flip extend) empty

-- limpa repetições nas formulas binarias
clean  :: Eq a => [a] -> [a] -> [a]
clean vs ws = vs ++ [w | w<- ws, not (elem w vs)]

-- obtem todas as variaveis em uma fórmula
vars :: Prop -> String
vars T = ""
vars F = ""
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = clean (vars p) (vars q)
vars (Or p q) = clean (vars p) (vars q)
vars (Imp p q) = clean (vars p) (vars q)

-- avaliação de satisfatibilidade
eval :: ValFn -> Prop -> Bool
eval _ T            = True
eval _ F            = False
eval (_, v) (Var p) = v p
eval phi (And p q)  = eval phi p && eval phi q
eval phi (Or p q)   = eval phi p || eval phi q
eval phi (Imp p q)  = eval phi (Or (Not p) q)
eval phi (Not p)    = not (eval phi p)

-- gera todas as valorações (linhas da tabela verdade)
allValFns :: String -> [ValFn]
allValFns [] = [empty]
allValFns (x:xs) = [extend v (x, b) | v <- vs, b <- [True, False]]
  where vs = allValFns xs

-- verifica se é tautologia
isTaut :: Prop -> Bool
isTaut p = and [eval phi p | phi <- allValFns (vars p)]

--verifica se é satisfazivel
isSat :: Prop -> Bool
isSat p = or [eval phi p | phi <- allValFns (vars p)]

-- filtra as linhas da tabela que são T
satValFns :: Prop -> [ValFn]
satValFns p = [phi | phi <- allValFns (vars p), eval phi p]

main :: IO ()
main = do
  let p1 = And (Or (Var 'A') (Var 'B')) (Not (Var 'A'))
      phi = table2valFn [('A', True), ('B', False)]
  print (isTaut p1)
  print (isSat p1)
  print ([valFn2table phi | phi <- satValFns p1])
