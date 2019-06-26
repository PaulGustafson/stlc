module Lib where


data UExp v                       -- untyped expression of variable type v
  = UVar v                        -- variable
  | ULit Bool                     -- literal
  | ULam v (UExp v)               -- lambda
  | UApp (UExp v) (UExp v)        -- apply
  deriving (Show)

data Ty                           -- types
  = Atom 
  | Fun Ty Ty
  deriving (Show, Eq)

data TExp v                       -- typed expressions
  = TVar v Ty
  | TLit Bool Atom
  | TLam v (TExp v) Ty Ty
  | TApp (TExp v) (TExp v) Ty


data PExp v                       -- partially typed expression
  = PVar v (Maybe Ty)
  | PLit Bool Atom
  | PLam v (PExp v) (Maybe Ty) (Maybe Ty)
  | PApp (TExp v) (TExp v) (Maybe Ty)


type Context v b = v -> Maybe (TyA b)

type Failable t = Either t String



-- type Exp = ExpA String 

-- instance Show Exp where
--     show (Var s) = s
--     show (Lam s e) = "\\" + s + " -> " + (show e)
--     show (App e1 e2) = "(" + (show e1) +") (" + (show e2) + ")"



-- append :: Eq v => (v, TyA b) ->  Context v b -> Context v b
-- append h c = \ x -> case (c x) of
--   Just a -> Just a
--   Nothing -> if x == fst h
--     then Just (snd h)
--     else Nothing


-- -- Ascribes as many types as possible, given a context
-- ascribe :: (Eq v, Show v, Show b) => Context v b -> ExpA v b -> Failable (ExpA v b)
-- ascribe c e = case e of
--   Var x -> case (c x) of
--     Just a -> if check e a
--               then Left $ Asc e a
--               else Right $ "Type mismatch " ++ (show e) ++ "does not have type " ++ (show a)
--     Nothing -> e
--   Lit l -> Left (Asc e Atom)
--   Lam x e2 -> case (synth e) of
--     Just f -> case f of
--       Fun a b -> Left $ Lam x (ascribe (append (x, a) c) e2)
--       _       -> Right $ "Expected function"
--     Nothing -> Lam x (ascribe c e2)
--   App e1 e2 -> case (synth e) of
--     Just a -> Left $ Asc (App (ascribe c e1) (ascribe c e2)) a
--     Nothing -> Left $ App (ascribe c e1) (ascribe c e2)
--   Asc e2 t  -> if check e2 t
--                then case t of
--                       Atom -> e
--                       Fun a d -> Left $ Asc (ascribe (a
  
  
    
    

-- synth :: ExpA v b ->  Maybe (TyA b)
-- synth (Var x) = Nothing
-- synth (Lit x) = Just Atom
-- synth (Lam x e) = Nothing 
-- synth (App x y) = case (synth x) of
--   Just tx -> case tx of
--     Fun a c -> if (check y a)
--       then (Just c)
--       else Nothing
--     _ -> Nothing
--   _ -> Nothing
-- synth (Asc x y) = case (check x y) of
--   True -> Just y
--   False -> Nothing

-- check :: ExpA v b -> TyA b -> Bool
-- check (Var x) y = True
-- check (Lit x) y = case y of 
--      Atom -> True
--      _    -> False
-- check (Lam x e) y = case y of
--      Fun a c -> check e c 
--      _       -> False
-- check (App x y) z = case x of
--      Lam w e -> case (synth x) of
--        Just f -> case f of
--          Fun a c -> (check y a) && (c == z)
--          _       -> False
--        _       -> False
--      _       -> False
-- check (Asc x y) z = y == z

-- TODO: replace == with unification
-- TODO: Use dependent types to replace bool with a base type enumeration




