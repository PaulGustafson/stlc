module Lib where

data ExpA v b                      -- expression of variable type v
                                   --    and base type b
  = Var v                          -- variable
  | Lit b                          -- literal
  | Lam v (ExpA v b)               -- lambda
  | App (ExpA v b) (ExpA v b)      -- apply
  | Asc (ExpA v b) (TyA b)         -- ascription
  deriving (Show)

type Exp = ExpA String Bool

-- instance Show Exp where
--     show (Var s) = s
--     show (Lam s e) = "\\" + s + " -> " + (show e)
--     show (App e1 e2) = "(" + (show e1) +") (" + (show e2) + ")"

data TyA b                 -- Types over a base type b
  = Atom 
  | Fun (TyA b) (TyA b)    -- function
  deriving (Show, Eq)

type Ty = TyA Bool

type Context v b = v -> Maybe (TyA b)

type Failable t = Either t String


append :: Eq v => (v, TyA b) ->  Context v b -> Context v b
append h c = \ x -> case (c x) of
  Just a -> Just a
  Nothing -> if x == fst h
    then Just (snd h)
    else Nothing


-- Ascribes as many types as possible, given a context
ascribe :: (Eq v, Show v, Show b) => Context v b -> ExpA v b -> Failable (ExpA v b)
ascribe c e = case e of
  Var x -> case (c x) of
    Just a -> if check e a
              then Left $ Asc e a
              else Right $ "Type mismatch " ++ (show e) ++ "does not have type " ++ (show a)
    Nothing -> e
  Lit l -> Left (Asc e Atom)
  Lam x e2 -> case (synth e) of
    Just f -> case f of
      Fun a b -> Left $ Lam x (ascribe (append (x, a) c) e2)
      _       -> Right $ "Expected function"
    Nothing -> Lam x (ascribe c e2)
  App e1 e2 -> case (synth e) of
    Just a -> Left $ Asc (App (ascribe c e1) (ascribe c e2)) a
    Nothing -> Left $ App (ascribe c e1) (ascribe c e2)
  Asc e2 t  -> if check e2 t
               then case t of
                      Atom -> e
                      Fun a d -> Left $ Asc (ascribe (append
  
  
    
    

synth :: ExpA v b ->  Maybe (TyA b)
synth (Var x) = Nothing
synth (Lit x) = Just Atom
synth (Lam x e) = Nothing 
synth (App x y) = case (synth x) of
  Just tx -> case tx of
    Fun a c -> if (check y a)
      then (Just c)
      else Nothing
    _ -> Nothing
  _ -> Nothing
synth (Asc x y) = case (check x y) of
  True -> Just y
  False -> Nothing

check :: ExpA v b -> TyA b -> Bool
check (Var x) y = True
check (Lit x) y = case y of 
     Atom -> True
     _    -> False
check (Lam x e) y = case y of
     Fun a c -> check e c 
     _       -> False
check (App x y) z = case x of
     Lam w e -> case (synth x) of
       Just f -> case f of
         Fun a c -> (check y a) && (c == z)
         _       -> False
       _       -> False
     _       -> False
check (Asc x y) z = y == z

-- TODO: replace == with unification





