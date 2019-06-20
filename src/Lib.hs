module Lib where

data ExpA v t                      -- expression
  = Var v                          -- variable
  | Lit t                          -- literal
  | Lam v (ExpA v t)               -- lambda
  | App (ExpA v t) (ExpA v t)      -- apply
  | Asc (ExpA v t) (TyA t)         -- ascribe
  deriving (Show)

type Exp = ExpA String Bool

-- instance Show Exp where
--     show (Var s) = s
--     show (Lam s e) = "\\" + s + " -> " + (show e)
--     show (App e1 e2) = "(" + (show e1) +") (" + (show e2) + ")"

data TyA t                 -- Type
  = Atom 
  | Fun (TyA t) (TyA t)    -- function
  deriving (Show, Eq)

type Ty = TyA Bool

type Context v t = v -> Maybe t

append :: Eq v => (v, t) ->  Context v t -> Context v t
append h c = \ x -> case (c x) of
  Just a -> Just a
  Nothing -> if x == fst h
    then Just (snd h)
    else Nothing


-- Ascribes as many types as possible, given a context
ascribe :: Context v t -> ExpA v t -> ExpA v t
ascribe c e = case e of
  Var x -> case (c x) of
    Just a -> Asc e a  --FIXME
    Nothing -> e
  Lam x e2 -> case (synth e) of
    Just f -> case f of
      Fun a b -> Lam x (ascribe (append (x, a) c) e2)
      _       -> error "Expected function"
    _ -> e
  App e1 e2 -> case (synth e) of
    Just a -> Asc (App (ascribe c e1) (ascribe c e2)) a
    Nothing -> e
  _ -> e
    
    

synth :: ExpA v t -> Maybe (TyA t)
synth (Var x) = Nothing
synth (Lit x) = Just Atom
synth (Lam x e) = Nothing 
synth (App x y) = case (synth x) of
  Just tx -> case tx of
    Fun a b -> if (check y a)
      then (Just b)
      else Nothing
    _ -> Nothing
  _ -> Nothing
synth (Asc x y) = case (check x y) of
  True -> Just y
  False -> Nothing

check :: ExpA v t -> TyA t -> Bool
check (Var x) y = True
check (Lit x) y = case y of 
     Atom -> True
     _    -> False
check (Lam x e) y = case y of
     Fun a b -> check e b 
     _       -> False
check (App x y) z = case x of
     Lam w e -> case (synth x) of
       Just f -> case f of
         Fun a b -> (check y a) && (b == z)
         _       -> False
       _       -> False
     _       -> False
check (Asc x y) z = y == z

-- TODO: replace == with unification





