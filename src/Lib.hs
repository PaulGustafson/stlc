module Lib where

type IBool = Bool                 -- internal bool

data UExp v                       -- untyped expression of variable type v
  = UVar v                        -- variable
  | ULit IBool                     -- literal
  | ULam v (UExp v)               -- lambda
  | UApp (UExp v) (UExp v)        -- apply
  deriving (Show)

data Ty                           -- types
  = Atom 
  | Fun Ty Ty
  deriving (Show, Eq)

data TExp v                       -- typed expressions
  = TVar v Ty
  | TLit IBool 
  | TLam v (TExp v) Ty Ty
  | TApp (TExp v) (TExp v) Ty


data PExp v                       -- partially typed expression
  = PVar v (Maybe Ty)
  | PLit Bool 
  | PLam v (PExp v) (Maybe Ty) (Maybe Ty)
  | PApp (TExp v) (TExp v) (Maybe Ty)


type Context v = v -> Maybe Ty

type Error = String

type Failable t = Either t Error

-- type Exp = ExpA String 

-- instance Show Exp where
--     show (Var s) = s
--     show (Lam s e) = "\\" + s + " -> " + (show e)
--     show (App e1 e2) = "(" + (show e1) +") (" + (show e2) + ")"


insert :: Eq v => (v, Ty) ->  Context v -> Context v
insert h c = \ x ->
  if x == fst h
  then Just (snd h)
  else c x


typeof :: TExp v -> Ty
typeof (TVar x t) = t
typeof (TLit _) = Atom
typeof (TLam _ _ a b) = Fun a b
typeof (TApp _ _ t) = t


check :: Eq v => Context v -> TExp v -> Bool
check c (TVar x t) = case c x of
  Just t2 -> t2 == t
  Nothing -> True
check c (TLit _) = True
check c (TLam x e a b) = check (insert (x, a) c) e
check c (TApp f x z) = case f of
  TLam y e a b -> (a == typeof x) && (b == z) && (check c f) && (check c x)
  _ -> False



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

-- TODO: replace == with unification
-- TODO: Use dependent types to replace bool with a base type enumeration




